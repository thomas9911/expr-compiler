use cranelift::{codegen::data_value::DataValue, interpreter::step::ControlFlow};
use expr_compiler::analysis::{FunctionValueKindAnalysis, KindSet, ValueKind, ValueShape};
use expr_compiler::module::{CodegenBackend, CompileError, Module, llvm_backend_available};
use expr_compiler::parser::{Ast, BlockAst, FunctionDefAst, ParseLexer};
use expr_compiler::runtime::{
    build_argv_list_value, configure_runtime_arena, decode_int, reset_runtime_arena,
};
use expr_compiler::source::offset_to_line_col;
use expr_compiler::tokenizer::{Logos, Token};
use pico_args::Arguments;
use std::path::{Path, PathBuf};

const USAGE: &str = "usage: expr-compiler <source-file> [-o <output>] [--emit-ir] [--run-ir] [--run-jit] [--debug-types] [--backend <cranelift|llvm>] [--arena-mb <n>] [-- <arg>...]";

#[derive(Debug)]
struct CliArgs {
    input: PathBuf,
    output: Option<PathBuf>,
    emit_ir: bool,
    run_ir: bool,
    run_jit: bool,
    debug_types: bool,
    backend: CodegenBackend,
    arena_mb: usize,
    program_args: Vec<String>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum OutputKind {
    Native,
    Wasm,
    Component,
}

fn finalize_output_path(mut output: PathBuf) -> PathBuf {
    if cfg!(windows) && output.extension().is_none() {
        output.set_extension("exe");
    }
    output
}

fn classify_output(output: Option<&Path>) -> OutputKind {
    let wants_wasm = output
        .and_then(|path| path.extension())
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("wasm"));
    let wants_component = output
        .and_then(|path| path.file_name())
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with(".component.wasm"));
    if wants_component {
        OutputKind::Component
    } else if wants_wasm {
        OutputKind::Wasm
    } else {
        OutputKind::Native
    }
}

fn validate_cli_runtime(
    cli: &CliArgs,
    llvm_available: bool,
    wasi_available: bool,
) -> Result<OutputKind, String> {
    if cli.backend == CodegenBackend::Llvm && !llvm_available {
        return Err(
            "llvm backend is not available in this build; enable the `llvm-backend` cargo feature"
                .to_string(),
        );
    }

    if cli.backend == CodegenBackend::Llvm && (cli.emit_ir || cli.run_ir) {
        return Err("llvm backend does not support --emit-ir or --run-ir".to_string());
    }

    let output_kind = classify_output(cli.output.as_deref());
    if output_kind == OutputKind::Wasm && cli.backend != CodegenBackend::Llvm {
        return Err("wasm output currently supports only --backend llvm".to_string());
    }
    if output_kind == OutputKind::Component && !wasi_available {
        return Err(
            "component wasm output requires building with the `wasi` cargo feature".to_string()
        );
    }

    Ok(output_kind)
}

fn parse_cli_args() -> Result<CliArgs, String> {
    parse_cli_args_from(std::env::args_os().skip(1).collect::<Vec<_>>())
}

fn parse_cli_args_from<I, T>(args: I) -> Result<CliArgs, String>
where
    I: IntoIterator<Item = T>,
    T: Into<std::ffi::OsString> + Clone,
{
    let mut args = Arguments::from_vec(args.into_iter().map(Into::into).collect());

    if args.contains(["-h", "--help"]) {
        return Err(USAGE.to_string());
    }

    let output = args
        .opt_value_from_str::<_, String>("-o")
        .map_err(|e| format!("failed to parse -o: {e}"))?
        .map(PathBuf::from);

    let emit_ir = args.contains("--emit-ir");
    let run_ir = args.contains("--run-ir");
    let run_jit = args.contains("--run-jit");
    let debug_types = args.contains("--debug-types");
    let backend = args
        .opt_value_from_str::<_, String>("--backend")
        .map_err(|e| format!("failed to parse --backend: {e}"))?
        .map(|value| value.parse())
        .transpose()?
        .unwrap_or(CodegenBackend::Cranelift);
    let arena_mb = args
        .opt_value_from_str::<_, usize>("--arena-mb")
        .map_err(|e| format!("failed to parse --arena-mb: {e}"))?
        .unwrap_or(16);
    if arena_mb == 0 {
        return Err("--arena-mb must be > 0".to_string());
    }

    let input = args.free_from_str::<String>().map_err(|_| USAGE.to_string())?;

    let mut program_args =
        args.finish().into_iter().map(|x| x.to_string_lossy().to_string()).collect::<Vec<_>>();
    if matches!(program_args.first().map(String::as_str), Some("--")) {
        program_args.remove(0);
    }
    if !run_jit && !program_args.is_empty() {
        let unknown = program_args.join(" ");
        return Err(format!("unknown arguments: {unknown}\n{USAGE}"));
    }

    Ok(CliArgs {
        input: PathBuf::from(input),
        output,
        emit_ir,
        run_ir,
        run_jit,
        debug_types,
        backend,
        arena_mb,
        program_args,
    })
}

fn print_cli_error_and_exit(err: &str) -> ! {
    eprintln!("{err}");
    let code = if err == USAGE { 0 } else { 1 };
    std::process::exit(code);
}

fn read_source_or_exit(input: &Path) -> String {
    std::fs::read_to_string(input).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", input.display());
        std::process::exit(1);
    })
}

fn write_ir_or_stdout(ir: &str, output: Option<&Path>) {
    if let Some(output) = output {
        std::fs::write(output, ir).unwrap_or_else(|e| {
            eprintln!("error writing IR: {e}");
            std::process::exit(1);
        });
    } else {
        print!("{ir}");
    }
}

fn format_kind_set(kinds: KindSet) -> String {
    if kinds.is_empty() {
        return "unknown".to_string();
    }

    let mut names = vec![];
    for (kind, label) in [
        (ValueKind::Int, "int"),
        (ValueKind::BigInt, "bigint"),
        (ValueKind::String, "string"),
        (ValueKind::List, "list"),
        (ValueKind::Function, "function"),
        (ValueKind::StringIter, "string_iter"),
    ] {
        if kinds.contains(kind) {
            names.push(label);
        }
    }

    if names.is_empty() { "unknown".to_string() } else { names.join(" | ") }
}

fn format_value_shape(shape: &ValueShape) -> String {
    if shape.arity() == 1 {
        return shape
            .slot(0)
            .map(format_kind_set)
            .unwrap_or_else(|| "unknown".to_string());
    }

    let slots =
        shape.slots().iter().copied().map(format_kind_set).collect::<Vec<_>>().join(", ");
    format!("({slots})")
}

fn add_annotation(
    annotations: &mut std::collections::BTreeMap<usize, Vec<String>>,
    line: usize,
    text: String,
) {
    annotations.entry(line).or_default().push(text);
}

fn collect_block_type_annotations(
    source: &str,
    block: &BlockAst,
    analysis: &FunctionValueKindAnalysis,
    annotations: &mut std::collections::BTreeMap<usize, Vec<String>>,
) {
    for line in &block.lines {
        collect_ast_type_annotations(source, line, analysis, annotations);
    }
}

fn collect_ast_type_annotations(
    source: &str,
    ast: &Ast,
    analysis: &FunctionValueKindAnalysis,
    annotations: &mut std::collections::BTreeMap<usize, Vec<String>>,
) {
    match ast {
        Ast::Assign { name, span, .. } => {
            if let (Some(span), Some(kinds)) = (span.as_ref(), analysis.variables.get(name)) {
                add_annotation(
                    annotations,
                    offset_to_line_col(source, span.start).line,
                    format!("{name}: {}", format_kind_set(*kinds)),
                );
            }
        }
        Ast::MultiAssign { names, span, .. } => {
            if let Some(span) = span.as_ref() {
                let vars = names
                    .iter()
                    .filter_map(|name| {
                        analysis.variables.get(name).map(|kinds| format!("{name}: {}", format_kind_set(*kinds)))
                    })
                    .collect::<Vec<_>>();
                if !vars.is_empty() {
                    add_annotation(
                        annotations,
                        offset_to_line_col(source, span.start).line,
                        vars.join(", "),
                    );
                }
            }
        }
        Ast::If { then, else_, .. } => {
            collect_block_type_annotations(source, then, analysis, annotations);
            if let Some(else_block) = else_ {
                collect_block_type_annotations(source, else_block, analysis, annotations);
            }
        }
        Ast::Block(block) => collect_block_type_annotations(source, block, analysis, annotations),
        Ast::IndexAssign { value, .. } => collect_ast_type_annotations(source, value, analysis, annotations),
        _ => {}
    }
}

fn format_function_annotation(function: &FunctionDefAst, analysis: &FunctionValueKindAnalysis) -> String {
    let mut parts = vec![];
    if !function.inputs.is_empty() {
        let inputs = function
            .inputs
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let kinds = analysis.inputs.get(index).copied().unwrap_or_else(KindSet::empty);
                format!("{name}: {}", format_kind_set(kinds))
            })
            .collect::<Vec<_>>()
            .join(", ");
        parts.push(format!("inputs [{inputs}]"));
    }
    parts.push(format!("returns {}", format_value_shape(&analysis.returns)));
    parts.join("; ")
}

fn parse_user_functions_for_debug(source: &str) -> Result<Vec<FunctionDefAst>, CompileError> {
    let lex = Token::lexer(source);
    let mut lexer = ParseLexer::new(lex);
    let mut functions = vec![];
    loop {
        while lexer.peek() == Some(&Ok(Token::Newline)) {
            lexer.next();
        }
        if lexer.peek().is_none() {
            break;
        }
        match Ast::from_lexer(&mut lexer) {
            Ok(Ast::FunctionDef(func)) => functions.push(func),
            Ok(_) => return Err(CompileError::TopLevelExpression),
            Err(err) => {
                return Err(CompileError::Parse {
                    message: err.to_string(),
                    span: Some(err.span),
                });
            }
        }
    }
    Ok(functions)
}

fn format_debug_types(
    source: &str,
    user_functions: &[FunctionDefAst],
    module: &Module,
) -> Result<String, CompileError> {
    let analysis = module.analyze_value_kinds()?;
    let mut annotations = std::collections::BTreeMap::<usize, Vec<String>>::new();
    for function in user_functions {
        let Some(function_analysis) = analysis.functions.get(&function.name) else { continue };
        if let Some(span) = function.span.as_ref() {
            add_annotation(
                &mut annotations,
                offset_to_line_col(source, span.start).line,
                format_function_annotation(function, function_analysis),
            );
        }
        collect_block_type_annotations(source, &function.block, function_analysis, &mut annotations);
    }

    let mut rendered = String::new();
    for (index, line) in source.lines().enumerate() {
        let line_no = index + 1;
        rendered.push_str(line);
        rendered.push('\n');
        if let Some(items) = annotations.get(&line_no) {
            for item in items {
                rendered.push_str("#? ");
                rendered.push_str(item);
                rendered.push('\n');
            }
        }
    }
    Ok(rendered)
}

fn format_compile_error(path: &Path, source: &str, err: &CompileError) -> String {
    if let Some(span) = err.span() {
        let pos = offset_to_line_col(source, span.start);
        let snippet = render_source_snippet(source, span);
        format!("{}:{}:{}: {err}\n{snippet}", path.display(), pos.line, pos.column)
    } else {
        err.to_string()
    }
}

fn render_source_snippet(source: &str, span: &expr_compiler::source::Span) -> String {
    let line_start = source[..span.start.min(source.len())].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_end = source[span.start.min(source.len())..]
        .find('\n')
        .map(|i| span.start.min(source.len()) + i)
        .unwrap_or(source.len());
    let line_text = &source[line_start..line_end];

    let start = span.start.min(line_end);
    let end = span.end.min(line_end).max(start);
    let start_col = offset_to_line_col(source, start).column;
    let end_col = offset_to_line_col(source, end).column;
    let underline_width = (end_col.saturating_sub(start_col)).max(1);

    let line_no = offset_to_line_col(source, span.start.min(source.len())).line;
    let gutter = line_no.to_string();
    let underline =
        format!("{}{}", " ".repeat(start_col.saturating_sub(1)), "^".repeat(underline_width));

    format!("{gutter} | {line_text}\n{} | {underline}", " ".repeat(gutter.len()))
}

fn run_ir(ir: &str) -> Result<Option<i64>, String> {
    let functions =
        cranelift_reader::parse_functions(ir).map_err(|e| format!("error parsing IR: {e}"))?;
    let mut function_store = cranelift::interpreter::environment::FunctionStore::default();
    let mut fallback_func = None;
    let mut main_func = None;
    let mut function_param_counts = std::collections::HashMap::new();
    for func in functions.iter() {
        let name = func.name.to_string();
        if name == "main" {
            main_func = Some(name.clone());
        }
        fallback_func = Some(name.clone());
        function_param_counts.insert(name.clone(), func.signature.params.len());
        function_store.add(name, func);
    }
    let state = cranelift::interpreter::interpreter::InterpreterState::default()
        .with_function_store(function_store);
    let mut interpreter = cranelift::interpreter::interpreter::Interpreter::new(state);
    if let Some(func_name) = main_func.or(fallback_func) {
        let args = vec![DataValue::I64(0); function_param_counts[&func_name]];
        match interpreter.call_by_name(&func_name, &args) {
            Ok(ControlFlow::Return(res)) => {
                if let Some(DataValue::I64(x)) = res.first() {
                    return Ok(Some(decode_int(*x).unwrap_or(*x)));
                }
            }
            Ok(ControlFlow::Trap(trap)) => {
                return Err(format!("trap: {trap:?}"));
            }
            Err(e) => {
                return Err(format!("interpreter error: {e}"));
            }
            _ => {}
        }
    }
    Ok(None)
}

fn maybe_handle_ir_modes(cli: &CliArgs, input: &Path, source: &str) -> Result<bool, String> {
    if !cli.emit_ir && !cli.run_ir {
        return Ok(false);
    }

    let ir = Module::try_from_source(source)
        .map_err(|err| format_compile_error(input, source, &err))?
        .try_compile_to_ir()
        .map_err(|err| format_compile_error(input, source, &err))?;
    if cli.emit_ir {
        write_ir_or_stdout(&ir, cli.output.as_deref());
    }
    if cli.run_ir {
        match run_ir(&ir) {
            Ok(Some(value)) => println!("{value}"),
            Ok(None) => {}
            Err(err) => return Err(err),
        }
    }
    Ok(!cli.run_jit)
}

fn maybe_handle_debug_types(cli: &CliArgs, input: &Path, source: &str) -> Result<bool, String> {
    if !cli.debug_types {
        return Ok(false);
    }

    let module =
        Module::try_from_source(source).map_err(|err| format_compile_error(input, source, &err))?;
    let user_functions =
        parse_user_functions_for_debug(source).map_err(|err| format_compile_error(input, source, &err))?;
    let rendered = format_debug_types(source, &user_functions, &module)
        .map_err(|err| format_compile_error(input, source, &err))?;
    print!("{rendered}");
    Ok(true)
}

fn select_jit_entry(module: &Module) -> Result<(String, usize), String> {
    if let Some(main) = module.functions.iter().find(|func| func.name == "main") {
        return Ok((main.name.clone(), main.inputs.len()));
    }

    let func = module.functions.first().ok_or_else(|| "no functions found".to_string())?;
    Ok((func.name.clone(), func.inputs.len()))
}

fn run_jit(cli: &CliArgs, input: &Path, source: &str) -> Result<i32, String> {
    configure_runtime_arena(cli.arena_mb * 1024 * 1024);
    reset_runtime_arena();
    let module =
        Module::try_from_source(source).map_err(|err| format_compile_error(input, source, &err))?;
    let (func_name, func_arity) = select_jit_entry(&module)?;
    if func_arity > 1 {
        return Err("jit entry function supports at most one argument".to_string());
    }

    let jit = module
        .try_compile_to_jit_with_backend(cli.backend)
        .map_err(|err| format_compile_error(input, source, &err))?;
    let int_result = if let Some(ptr) = jit.get_int_result_fn_ptr(&func_name) {
        if func_arity == 1 {
            let (arg_tag, arg_payload) = build_argv_list_value(&cli.program_args);
            let func =
                unsafe { std::mem::transmute::<*const u8, extern "C" fn(i64, i64) -> i64>(ptr) };
            func(arg_tag, arg_payload)
        } else {
            let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
            func()
        }
    } else {
        let ptr = jit.get_fn_ptr(&func_name);
        let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
        let result = func();
        decode_int(result)
            .ok_or_else(|| "runtime error: main returned non-integer value".to_string())?
    };
    Ok(int_result as i32)
}

fn compile_native_or_exit(cli: &CliArgs, input: &Path, source: &str) {
    let output = finalize_output_path(
        cli.output.clone().unwrap_or_else(|| input.with_extension("").to_path_buf()),
    );

    Module::try_from_source(source)
        .and_then(|module| module.try_compile_to_executable_with_backend(&output, cli.backend))
        .unwrap_or_else(|err| {
            eprintln!("{}", format_compile_error(input, source, &err));
            std::process::exit(1);
        });
    println!("compiled to {}", output.display());
}

fn main() {
    let cli = match parse_cli_args() {
        Ok(cli) => cli,
        Err(err) => print_cli_error_and_exit(&err),
    };

    let input = Path::new(&cli.input);
    let source = read_source_or_exit(input);

    match maybe_handle_debug_types(&cli, input, &source) {
        Ok(true) => return,
        Ok(false) => {}
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }

    if let Err(err) = validate_cli_runtime(&cli, llvm_backend_available(), cfg!(feature = "wasi")) {
        print_cli_error_and_exit(&err);
    }

    match maybe_handle_ir_modes(&cli, input, &source) {
        Ok(true) => return,
        Ok(false) => {}
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }

    if cli.run_jit {
        match run_jit(&cli, input, &source) {
            Ok(code) => std::process::exit(code),
            Err(err) => {
                eprintln!("{err}");
                std::process::exit(1);
            }
        }
    }

    compile_native_or_exit(&cli, input, &source);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(args: &[&str]) -> CliArgs {
        parse_cli_args_from(args.iter().copied()).expect("cli parse should succeed")
    }

    #[test]
    fn cli_defaults_to_cranelift() {
        let cli = parse_ok(&["examples/test.expr"]);
        assert_eq!(cli.backend, CodegenBackend::Cranelift);
        assert_eq!(cli.arena_mb, 16);
        assert!(!cli.run_jit);
        assert!(!cli.debug_types);
    }

    #[test]
    fn cli_accepts_llvm_backend() {
        let cli = parse_ok(&["examples/test.expr", "--run-jit", "--backend", "llvm"]);
        assert_eq!(cli.backend, CodegenBackend::Llvm);
        assert!(cli.run_jit);
    }

    #[test]
    fn cli_collects_program_args_for_run_jit() {
        let cli = parse_ok(&["examples/test.expr", "--run-jit", "--", "hello", "world"]);
        assert_eq!(cli.program_args, vec!["hello", "world"]);
    }

    #[test]
    fn cli_accepts_debug_types() {
        let cli = parse_ok(&["examples/test.expr", "--debug-types"]);
        assert!(cli.debug_types);
    }

    #[test]
    fn cli_rejects_unknown_backend() {
        let err = parse_cli_args_from(["examples/test.expr", "--backend", "nope"])
            .expect_err("cli parse should fail");
        assert!(err.contains("unknown backend: nope"));
    }

    #[test]
    fn cli_rejects_zero_arena() {
        let err = parse_cli_args_from(["examples/test.expr", "--arena-mb", "0"])
            .expect_err("cli parse should fail");
        assert_eq!(err, "--arena-mb must be > 0");
    }

    #[test]
    fn cli_rejects_program_args_without_run_jit() {
        let err = parse_cli_args_from(["examples/test.expr", "--", "hello"])
            .expect_err("cli parse should fail");
        assert!(err.contains("unknown arguments: hello"));
    }

    #[test]
    fn cli_help_returns_usage() {
        let err =
            parse_cli_args_from(["--help"]).expect_err("help should short-circuit with usage");
        assert_eq!(err, USAGE);
    }

    #[test]
    fn finalize_output_adds_windows_extension_only_when_missing() {
        let input = PathBuf::from("demo");
        let output = finalize_output_path(input);
        if cfg!(windows) {
            assert_eq!(output, PathBuf::from("demo.exe"));
        } else {
            assert_eq!(output, PathBuf::from("demo"));
        }
    }

    #[test]
    fn finalize_output_keeps_existing_extension() {
        assert_eq!(finalize_output_path(PathBuf::from("demo.bin")), PathBuf::from("demo.bin"));
    }

    #[test]
    fn classify_output_detects_native() {
        assert_eq!(classify_output(None), OutputKind::Native);
        assert_eq!(classify_output(Some(Path::new("examples/out"))), OutputKind::Native);
    }

    #[test]
    fn classify_output_detects_wasm() {
        assert_eq!(classify_output(Some(Path::new("examples/out.wasm"))), OutputKind::Wasm);
    }

    #[test]
    fn classify_output_detects_component_wasm() {
        assert_eq!(
            classify_output(Some(Path::new("examples/out.component.wasm"))),
            OutputKind::Component
        );
    }

    #[test]
    fn validate_cli_rejects_missing_llvm_backend() {
        let cli = parse_ok(&["examples/test.expr", "--backend", "llvm"]);
        let err = validate_cli_runtime(&cli, false, true)
            .expect_err("llvm-less build should reject llvm backend");
        assert!(err.contains("llvm backend is not available"));
    }

    #[test]
    fn validate_cli_rejects_emit_ir_with_llvm_backend() {
        let cli = parse_ok(&["examples/test.expr", "--backend", "llvm", "--emit-ir"]);
        let err =
            validate_cli_runtime(&cli, true, true).expect_err("llvm backend should reject emit-ir");
        assert_eq!(err, "llvm backend does not support --emit-ir or --run-ir");
    }

    #[test]
    fn validate_cli_rejects_run_ir_with_llvm_backend() {
        let cli = parse_ok(&["examples/test.expr", "--backend", "llvm", "--run-ir"]);
        let err =
            validate_cli_runtime(&cli, true, true).expect_err("llvm backend should reject run-ir");
        assert_eq!(err, "llvm backend does not support --emit-ir or --run-ir");
    }

    #[test]
    fn validate_cli_rejects_wasm_output_without_llvm_backend() {
        let cli = parse_ok(&["examples/test.expr", "-o", "out.wasm"]);
        let err = validate_cli_runtime(&cli, true, true)
            .expect_err("cranelift should reject wasm output");
        assert_eq!(err, "wasm output currently supports only --backend llvm");
    }

    #[test]
    fn validate_cli_rejects_component_output_without_wasi_feature() {
        let cli =
            parse_ok(&["examples/test.expr", "--backend", "llvm", "-o", "out.component.wasm"]);
        let err = validate_cli_runtime(&cli, true, false)
            .expect_err("component output should require wasi");
        assert_eq!(err, "component wasm output requires building with the `wasi` cargo feature");
    }

    #[test]
    fn validate_cli_accepts_component_output_when_available() {
        let cli =
            parse_ok(&["examples/test.expr", "--backend", "llvm", "-o", "out.component.wasm"]);
        let kind =
            validate_cli_runtime(&cli, true, true).expect("component output should be accepted");
        assert_eq!(kind, OutputKind::Component);
    }

    #[test]
    fn validate_cli_accepts_wasm_output_with_llvm_backend() {
        let cli = parse_ok(&["examples/test.expr", "--backend", "llvm", "-o", "out.wasm"]);
        let kind = validate_cli_runtime(&cli, true, true).expect("wasm output should be accepted");
        assert_eq!(kind, OutputKind::Wasm);
    }

    #[test]
    fn run_ir_executes_compiled_program() {
        let ir = "function %main() -> i64 system_v {\nblock0:\n    v0 = iconst.i64 7\n    return v0\n}\n";
        let result = run_ir(&ir).expect("run_ir should succeed");
        assert_eq!(result, Some(7));
    }

    #[test]
    fn maybe_handle_ir_modes_returns_false_when_ir_modes_are_disabled() {
        let cli = parse_ok(&["examples/test.expr"]);
        let handled =
            maybe_handle_ir_modes(&cli, Path::new("input.expr"), "fn main() do\n    7\nend")
                .expect("ir handling should not fail");
        assert!(!handled);
    }

    #[test]
    fn maybe_handle_debug_types_returns_false_when_disabled() {
        let cli = parse_ok(&["examples/test.expr"]);
        let handled =
            maybe_handle_debug_types(&cli, Path::new("input.expr"), "fn main() do\n    7\nend")
                .expect("debug-types handling should not fail");
        assert!(!handled);
    }

    #[test]
    fn maybe_handle_ir_modes_writes_ir_file_and_returns_true() {
        let unique = format!(
            "expr-compiler-main-test-{}.clif",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("system time should be after unix epoch")
                .as_nanos()
        );
        let output = std::env::temp_dir().join(unique);
        let output_str = output.to_string_lossy().into_owned();
        let cli = parse_ok(&["examples/test.expr", "--emit-ir", "-o", &output_str]);

        let handled =
            maybe_handle_ir_modes(&cli, Path::new("input.expr"), "fn main() do\n    7\nend")
                .expect("emit-ir handling should succeed");
        assert!(handled);

        let ir = std::fs::read_to_string(&output).expect("ir output should exist");
        assert!(ir.contains("function"));

        let _ = std::fs::remove_file(output);
    }

    #[test]
    fn maybe_handle_ir_modes_returns_false_when_run_jit_is_also_requested() {
        let cli = parse_ok(&["examples/test.expr", "--emit-ir", "--run-jit"]);
        let handled =
            maybe_handle_ir_modes(&cli, Path::new("input.expr"), "fn main() do\n    7\nend")
                .expect("emit-ir handling should succeed");
        assert!(!handled);
    }

    #[test]
    fn maybe_handle_ir_modes_propagates_run_ir_errors() {
        let cli = parse_ok(&["examples/test.expr", "--run-ir"]);
        let err =
            maybe_handle_ir_modes(&cli, Path::new("input.expr"), "fn main() do\n    1 / 0\nend")
                .expect_err("run-ir should propagate traps as errors");
        assert!(!err.is_empty());
    }

    #[test]
    fn format_debug_types_includes_function_and_assignment_annotations() {
        let source = "fn main() do\n    ok, value, err = string_try_parse_bigint(\"12\")\n    value\nend\n";
        let module = Module::try_from_source(source).expect("source should parse");
        let user_functions =
            parse_user_functions_for_debug(source).expect("user functions should parse");
        let rendered =
            format_debug_types(source, &user_functions, &module).expect("debug types should render");
        assert!(rendered.contains("#? returns bigint"));
        assert!(rendered.contains("#? ok: int, value: bigint, err: string"));
    }

    #[test]
    fn run_jit_executes_compiled_program() {
        let cli = parse_ok(&["examples/test.expr", "--run-jit"]);
        let result = run_jit(&cli, Path::new("input.expr"), "fn main() do\n    1 + 2 * 3\nend")
            .expect("run_jit should succeed");
        assert_eq!(result, 7);
    }

    #[test]
    fn run_jit_passes_program_args() {
        let cli = parse_ok(&["examples/test.expr", "--run-jit", "--", "hello", "world"]);
        let result = run_jit(
            &cli,
            Path::new("input.expr"),
            "fn main(args) do\n    list_len(args) + bytes_len(list_get(args, 0))\nend",
        )
        .expect("run_jit should succeed");
        assert_eq!(result, 7);
    }

    #[test]
    fn format_compile_error_includes_line_and_column_when_span_is_present() {
        let source = "fn main() do\n    missing_value\nend";
        let err = CompileError::UndefinedVariable {
            name: "missing_value".to_string(),
            span: Some(expr_compiler::source::Span { start: 17, end: 30 }),
        };
        assert_eq!(
            format_compile_error(Path::new("input.expr"), source, &err),
            "input.expr:2:5: undefined variable: missing_value\n2 |     missing_value\n  |     ^^^^^^^^^^^^^"
        );
    }

    #[test]
    fn format_compile_error_formats_parse_errors_with_line_and_column() {
        let source = "fn main()) do\n    1\nend";
        let err = CompileError::Parse {
            message: "unexpected token \")\"".to_string(),
            span: Some(expr_compiler::source::Span { start: 9, end: 10 }),
        };
        assert_eq!(
            format_compile_error(Path::new("input.expr"), source, &err),
            "input.expr:1:10: parse error: unexpected token \")\"\n1 | fn main()) do\n  |          ^"
        );
    }
}
