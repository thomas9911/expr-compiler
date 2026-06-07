use cranelift::{codegen::data_value::DataValue, interpreter::step::ControlFlow};
use expr_compiler::analysis::{FunctionValueKindAnalysis, KindSet, ValueKind, ValueShape};
use expr_compiler::format::{FormatConfig, format_source};
use expr_compiler::module::{CodegenBackend, CompileError, Module, llvm_backend_available};
use expr_compiler::parser::{Ast, BlockAst, FunctionDefAst, ParseLexer};
use expr_compiler::runtime::{
    build_argv_list_value, configure_runtime_arena, decode_int, reset_runtime_arena,
};
use expr_compiler::source::offset_to_line_col;
use expr_compiler::tokenizer::{Logos, Token};
use pico_args::Arguments;
use std::path::{Path, PathBuf};

const USAGE: &str = "usage:
  expr-compiler run <source-file> [--backend <cranelift|llvm>] [--arena-mb <n>] [-- <arg>...]
  expr-compiler build <source-file> [-o <output>] [--backend <cranelift|llvm>]
  expr-compiler wasm core <source-file> [-o <output>]
  expr-compiler wasm component <source-file> [-o <output>]
  expr-compiler ir <source-file> [-o <output>] [--run]
  expr-compiler types <source-file>
  expr-compiler fmt <source-path>
  expr-compiler format <source-path>";

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum WasmSubcommand {
    Core,
    Component,
}

#[derive(Debug)]
enum CliCommand {
    Run { input: PathBuf, backend: CodegenBackend, arena_mb: usize, program_args: Vec<String> },
    Build { input: PathBuf, output: Option<PathBuf>, backend: CodegenBackend },
    Wasm { target: WasmSubcommand, input: PathBuf, output: Option<PathBuf> },
    Ir { input: PathBuf, output: Option<PathBuf>, run: bool },
    Types { input: PathBuf },
    Format { input: PathBuf },
}

#[derive(Debug)]
struct CliArgs {
    command: CliCommand,
}

fn finalize_output_path(mut output: PathBuf) -> PathBuf {
    if cfg!(windows) && output.extension().is_none() {
        output.set_extension("exe");
    }
    output
}

fn parse_backend_arg(
    args: &mut Arguments,
    flag: &'static str,
    default: CodegenBackend,
) -> Result<CodegenBackend, String> {
    args.opt_value_from_str::<_, String>(flag)
        .map_err(|e| format!("failed to parse {flag}: {e}"))?
        .map(|value| value.parse())
        .transpose()?
        .map_or(Ok(default), Ok)
}

fn parse_output_arg(args: &mut Arguments) -> Result<Option<PathBuf>, String> {
    Ok(args
        .opt_value_from_str::<_, String>("-o")
        .map_err(|e| format!("failed to parse -o: {e}"))?
        .map(PathBuf::from))
}

fn parse_input_arg(args: &mut Arguments) -> Result<PathBuf, String> {
    args.free_from_str::<String>().map(PathBuf::from).map_err(|_| USAGE.to_string())
}

fn parse_arena_mb_arg(args: &mut Arguments) -> Result<usize, String> {
    let arena_mb = args
        .opt_value_from_str::<_, usize>("--arena-mb")
        .map_err(|e| format!("failed to parse --arena-mb: {e}"))?
        .unwrap_or(16);
    if arena_mb == 0 {
        return Err("--arena-mb must be > 0".to_string());
    }
    Ok(arena_mb)
}

fn parse_program_args(args: Arguments) -> Vec<String> {
    let mut program_args =
        args.finish().into_iter().map(|x| x.to_string_lossy().to_string()).collect::<Vec<_>>();
    if matches!(program_args.first().map(String::as_str), Some("--")) {
        program_args.remove(0);
    }
    program_args
}

fn reject_unknown_args(args: Arguments) -> Result<(), String> {
    let leftovers = args.finish();
    if leftovers.is_empty() {
        return Ok(());
    }
    let unknown = leftovers
        .into_iter()
        .map(|x| x.to_string_lossy().to_string())
        .collect::<Vec<_>>()
        .join(" ");
    Err(format!("unknown arguments: {unknown}\n{USAGE}"))
}

fn validate_cli_runtime(
    cli: &CliArgs,
    llvm_available: bool,
    wasi_available: bool,
) -> Result<(), String> {
    match &cli.command {
        CliCommand::Run { backend, .. } | CliCommand::Build { backend, .. } => {
            if *backend == CodegenBackend::Llvm && !llvm_available {
                return Err(
                    "llvm backend is not available in this build; enable the `llvm-backend` cargo feature"
                        .to_string(),
                );
            }
        }
        CliCommand::Wasm { target, .. } => {
            if !llvm_available {
                return Err(
                    "llvm backend is not available in this build; enable the `llvm-backend` cargo feature"
                        .to_string(),
                );
            }
            if *target == WasmSubcommand::Component && !wasi_available {
                return Err(
                    "component wasm output requires building with the `wasi` cargo feature"
                        .to_string(),
                );
            }
        }
        CliCommand::Ir { .. } | CliCommand::Types { .. } | CliCommand::Format { .. } => {}
    }
    Ok(())
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

    let command = args.free_from_str::<String>().map_err(|_| USAGE.to_string())?;
    let command = match command.as_str() {
        "run" => {
            let backend = parse_backend_arg(&mut args, "--backend", CodegenBackend::Cranelift)?;
            let arena_mb = parse_arena_mb_arg(&mut args)?;
            let input = parse_input_arg(&mut args)?;
            let program_args = parse_program_args(args);
            CliCommand::Run { input, backend, arena_mb, program_args }
        }
        "build" => {
            let output = parse_output_arg(&mut args)?;
            let backend = parse_backend_arg(&mut args, "--backend", CodegenBackend::Cranelift)?;
            let input = parse_input_arg(&mut args)?;
            reject_unknown_args(args)?;
            CliCommand::Build { input, output, backend }
        }
        "wasm" => {
            let target = args.free_from_str::<String>().map_err(|_| USAGE.to_string())?;
            let target = match target.as_str() {
                "core" => WasmSubcommand::Core,
                "component" => WasmSubcommand::Component,
                _ => return Err(format!("unknown wasm target: {target}\n{USAGE}")),
            };
            let output = parse_output_arg(&mut args)?;
            let input = parse_input_arg(&mut args)?;
            reject_unknown_args(args)?;
            CliCommand::Wasm { target, input, output }
        }
        "ir" => {
            let output = parse_output_arg(&mut args)?;
            let run = args.contains("--run");
            let input = parse_input_arg(&mut args)?;
            reject_unknown_args(args)?;
            CliCommand::Ir { input, output, run }
        }
        "types" => {
            let input = parse_input_arg(&mut args)?;
            reject_unknown_args(args)?;
            CliCommand::Types { input }
        }
        "fmt" | "format" => {
            let input = parse_input_arg(&mut args)?;
            reject_unknown_args(args)?;
            CliCommand::Format { input }
        }
        _ => return Err(format!("unknown command: {command}\n{USAGE}")),
    };

    Ok(CliArgs { command })
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
        (ValueKind::Map, "map"),
        (ValueKind::MapIter, "map_iter"),
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
    if let Some(items) = shape.list_items() {
        return format!("list<{}>", format_kind_set(items));
    }
    if let Some(values) = shape.map_values() {
        return format!("map<string, {}>", format_kind_set(values));
    }
    if let Some(values) = shape.map_iter_values() {
        return format!("map_iter<string, {}>", format_kind_set(values));
    }

    if shape.arity() == 1 {
        return shape.slot(0).map(format_kind_set).unwrap_or_else(|| "unknown".to_string());
    }

    let slots = (0..shape.arity())
        .filter_map(|index| shape.slot(index))
        .map(format_kind_set)
        .collect::<Vec<_>>()
        .join(", ");
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
            if let (Some(span), Some(shape)) = (span.as_ref(), analysis.variables.get(name)) {
                add_annotation(
                    annotations,
                    offset_to_line_col(source, span.start).line,
                    format!("{name}: {}", format_value_shape(shape)),
                );
            }
        }
        Ast::MultiAssign { names, span, .. } => {
            if let Some(span) = span.as_ref() {
                let vars = names
                    .iter()
                    .filter_map(|name| {
                        analysis
                            .variables
                            .get(name)
                            .map(|shape| format!("{name}: {}", format_value_shape(shape)))
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
        Ast::IndexAssign { value, .. } => {
            collect_ast_type_annotations(source, value, analysis, annotations)
        }
        _ => {}
    }
}

fn format_function_annotation(
    function: &FunctionDefAst,
    analysis: &FunctionValueKindAnalysis,
) -> String {
    let mut parts = vec![];
    if !function.inputs.is_empty() {
        let inputs = function
            .inputs
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let shape = analysis
                    .inputs
                    .get(index)
                    .cloned()
                    .unwrap_or_else(|| ValueShape::scalar(KindSet::empty()));
                format!("{name}: {}", format_value_shape(&shape))
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
                return Err(CompileError::Parse { message: err.to_string(), span: Some(err.span) });
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
        collect_block_type_annotations(
            source,
            &function.block,
            function_analysis,
            &mut annotations,
        );
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

fn format_file_in_place(input: &Path, source: &str) -> Result<(), String> {
    let formatted = format_source(source, &FormatConfig::inferred_from_source(source))
        .map_err(|err| format_compile_error(input, source, &err))?;
    std::fs::write(input, formatted)
        .map_err(|err| format!("error writing {}: {err}", input.display()))?;
    Ok(())
}

fn handle_format(input: &Path) -> Result<(), String> {
    let metadata = std::fs::metadata(input)
        .map_err(|err| format!("error reading {}: {err}", input.display()))?;
    if metadata.is_file() {
        let source = read_source_or_exit(input);
        return format_file_in_place(input, &source);
    }
    if metadata.is_dir() {
        let mut files = std::fs::read_dir(input)
            .map_err(|err| format!("error reading {}: {err}", input.display()))?
            .filter_map(|entry| entry.ok().map(|item| item.path()))
            .filter(|path| path.is_file() && path.extension().is_some_and(|ext| ext == "expr"))
            .collect::<Vec<_>>();
        files.sort();
        let mut had_errors = false;
        for file in files {
            let source = read_source_or_exit(&file);
            if let Err(err) = format_file_in_place(&file, &source) {
                eprintln!("{err}");
                had_errors = true;
            }
        }
        if had_errors {
            return Err(format!("one or more files failed to format in {}", input.display()));
        }
        return Ok(());
    }
    Err(format!("unsupported path type: {}", input.display()))
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

fn handle_ir(input: &Path, output: Option<&Path>, run: bool, source: &str) -> Result<(), String> {
    let ir = Module::try_from_source(source)
        .map_err(|err| format_compile_error(input, source, &err))?
        .try_compile_to_ir()
        .map_err(|err| format_compile_error(input, source, &err))?;
    if output.is_some() || !run {
        write_ir_or_stdout(&ir, output);
    }
    if run {
        match run_ir(&ir) {
            Ok(Some(value)) => println!("{value}"),
            Ok(None) => {}
            Err(err) => return Err(err),
        }
    }
    Ok(())
}

fn handle_debug_types(input: &Path, source: &str) -> Result<(), String> {
    let module =
        Module::try_from_source(source).map_err(|err| format_compile_error(input, source, &err))?;
    let user_functions = parse_user_functions_for_debug(source)
        .map_err(|err| format_compile_error(input, source, &err))?;
    let rendered = format_debug_types(source, &user_functions, &module)
        .map_err(|err| format_compile_error(input, source, &err))?;
    print!("{rendered}");
    Ok(())
}

fn select_jit_entry(module: &Module) -> Result<(String, usize), String> {
    if let Some(main) = module.functions.iter().find(|func| func.name == "main") {
        return Ok((main.name.clone(), main.inputs.len()));
    }

    let func = module.functions.first().ok_or_else(|| "no functions found".to_string())?;
    Ok((func.name.clone(), func.inputs.len()))
}

fn run_jit(
    input: &Path,
    source: &str,
    backend: CodegenBackend,
    arena_mb: usize,
    program_args: &[String],
) -> Result<i32, String> {
    configure_runtime_arena(arena_mb * 1024 * 1024);
    reset_runtime_arena();
    let module =
        Module::try_from_source(source).map_err(|err| format_compile_error(input, source, &err))?;
    let (func_name, func_arity) = select_jit_entry(&module)?;
    if func_arity > 1 {
        return Err("jit entry function supports at most one argument".to_string());
    }

    let jit = module
        .try_compile_to_jit_with_backend(backend)
        .map_err(|err| format_compile_error(input, source, &err))?;
    let int_result = if let Some(ptr) = jit.get_int_result_fn_ptr(&func_name) {
        if func_arity == 1 {
            let (arg_tag, arg_payload) = build_argv_list_value(program_args);
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

fn default_build_output(input: &Path) -> PathBuf {
    finalize_output_path(input.with_extension("").to_path_buf())
}

fn default_wasm_output(input: &Path, target: WasmSubcommand) -> PathBuf {
    match target {
        WasmSubcommand::Core => input.with_extension("wasm"),
        WasmSubcommand::Component => {
            let stem = input.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
            input.with_file_name(format!("{stem}.component.wasm"))
        }
    }
}

fn compile_native_or_exit(
    input: &Path,
    output: Option<&Path>,
    source: &str,
    backend: CodegenBackend,
) {
    let output = finalize_output_path(
        output.map(Path::to_path_buf).unwrap_or_else(|| default_build_output(input)),
    );
    Module::try_from_source(source)
        .and_then(|module| module.try_compile_to_executable_with_backend(&output, backend))
        .unwrap_or_else(|err| {
            eprintln!("{}", format_compile_error(input, source, &err));
            std::process::exit(1);
        });
    println!("compiled to {}", output.display());
}

fn compile_wasm_or_exit(input: &Path, output: Option<&Path>, target: WasmSubcommand, source: &str) {
    let output =
        output.map(Path::to_path_buf).unwrap_or_else(|| default_wasm_output(input, target));
    Module::try_from_source(source)
        .and_then(|module| {
            module.try_compile_to_executable_with_backend(&output, CodegenBackend::Llvm)
        })
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

    if let Err(err) = validate_cli_runtime(&cli, llvm_backend_available(), cfg!(feature = "wasi")) {
        print_cli_error_and_exit(&err);
    }

    match &cli.command {
        CliCommand::Run { input, backend, arena_mb, program_args } => {
            let input = Path::new(input);
            let source = read_source_or_exit(input);
            match run_jit(input, &source, *backend, *arena_mb, program_args) {
                Ok(code) => std::process::exit(code),
                Err(err) => {
                    eprintln!("{err}");
                    std::process::exit(1);
                }
            }
        }
        CliCommand::Build { input, output, backend } => {
            let input = Path::new(input);
            let source = read_source_or_exit(input);
            compile_native_or_exit(input, output.as_deref(), &source, *backend);
        }
        CliCommand::Wasm { target, input, output } => {
            let input = Path::new(input);
            let source = read_source_or_exit(input);
            compile_wasm_or_exit(input, output.as_deref(), *target, &source);
        }
        CliCommand::Ir { input, output, run } => {
            let input = Path::new(input);
            let source = read_source_or_exit(input);
            if let Err(err) = handle_ir(input, output.as_deref(), *run, &source) {
                eprintln!("{err}");
                std::process::exit(1);
            }
        }
        CliCommand::Types { input } => {
            let input = Path::new(input);
            let source = read_source_or_exit(input);
            if let Err(err) = handle_debug_types(input, &source) {
                eprintln!("{err}");
                std::process::exit(1);
            }
        }
        CliCommand::Format { input } => {
            let input = Path::new(input);
            if let Err(err) = handle_format(input) {
                eprintln!("{err}");
                std::process::exit(1);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(args: &[&str]) -> CliArgs {
        parse_cli_args_from(args.iter().copied()).expect("cli parse should succeed")
    }

    #[test]
    fn cli_run_defaults_to_cranelift() {
        let cli = parse_ok(&["run", "examples/test.expr"]);
        match cli.command {
            CliCommand::Run { input, backend, arena_mb, program_args } => {
                assert_eq!(input, PathBuf::from("examples/test.expr"));
                assert_eq!(backend, CodegenBackend::Cranelift);
                assert_eq!(arena_mb, 16);
                assert!(program_args.is_empty());
            }
            _ => panic!("expected run command"),
        }
    }

    #[test]
    fn cli_run_accepts_llvm_backend() {
        let cli = parse_ok(&["run", "examples/test.expr", "--backend", "llvm"]);
        match cli.command {
            CliCommand::Run { backend, .. } => assert_eq!(backend, CodegenBackend::Llvm),
            _ => panic!("expected run command"),
        }
    }

    #[test]
    fn cli_run_collects_program_args() {
        let cli = parse_ok(&["run", "examples/test.expr", "--", "hello", "world"]);
        match cli.command {
            CliCommand::Run { program_args, .. } => {
                assert_eq!(program_args, vec!["hello", "world"]);
            }
            _ => panic!("expected run command"),
        }
    }

    #[test]
    fn cli_types_parses() {
        let cli = parse_ok(&["types", "examples/test.expr"]);
        match cli.command {
            CliCommand::Types { input } => assert_eq!(input, PathBuf::from("examples/test.expr")),
            _ => panic!("expected types command"),
        }
    }

    #[test]
    fn cli_fmt_and_format_parse() {
        let cli = parse_ok(&["fmt", "examples/test.expr"]);
        match cli.command {
            CliCommand::Format { input } => {
                assert_eq!(input, PathBuf::from("examples/test.expr"));
            }
            _ => panic!("expected format command"),
        }

        let cli = parse_ok(&["format", "examples/test.expr"]);
        match cli.command {
            CliCommand::Format { input } => {
                assert_eq!(input, PathBuf::from("examples/test.expr"));
            }
            _ => panic!("expected format command"),
        }
    }

    #[test]
    fn handle_format_formats_all_expr_files_in_directory() {
        let temp_root = std::env::temp_dir().join(format!(
            "expr-compiler-format-dir-{}-{}",
            std::process::id(),
            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()
        ));
        std::fs::create_dir_all(&temp_root).unwrap();

        let first = temp_root.join("a.expr");
        let second = temp_root.join("b.expr");
        let ignored = temp_root.join("note.txt");

        std::fs::write(&first, "fn main():\n    x=1\n").unwrap();
        std::fs::write(&second, "fn main() do\n    y=2\nend\n").unwrap();
        std::fs::write(&ignored, "keep me                                           ").unwrap();

        handle_format(&temp_root).unwrap();

        assert_eq!(std::fs::read_to_string(&first).unwrap(), "fn main():\n    x = 1\n");
        assert_eq!(std::fs::read_to_string(&second).unwrap(), "fn main() do\n    y = 2\nend\n");
        assert_eq!(
            std::fs::read_to_string(&ignored).unwrap(),
            "keep me                                           "
        );

        std::fs::remove_dir_all(&temp_root).unwrap();
    }

    #[test]
    fn handle_format_continues_after_parse_error_in_directory() {
        let temp_root = std::env::temp_dir().join(format!(
            "expr-compiler-format-dir-error-{}-{}",
            std::process::id(),
            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos()
        ));
        std::fs::create_dir_all(&temp_root).unwrap();

        let valid = temp_root.join("a.expr");
        let invalid = temp_root.join("b.expr");

        std::fs::write(&valid, "fn main():\n    x=1\n").unwrap();
        std::fs::write(&invalid, "fn main() do\n    if\nend\n").unwrap();

        let err =
            handle_format(&temp_root).expect_err("directory formatting should report failure");
        assert!(err.contains("one or more files failed to format"));
        assert_eq!(std::fs::read_to_string(&valid).unwrap(), "fn main():\n    x = 1\n");

        std::fs::remove_dir_all(&temp_root).unwrap();
    }

    #[test]
    fn cli_wasm_component_parses() {
        let cli =
            parse_ok(&["wasm", "component", "examples/test.expr", "-o", "out.component.wasm"]);
        match cli.command {
            CliCommand::Wasm { target, input, output } => {
                assert_eq!(target, WasmSubcommand::Component);
                assert_eq!(input, PathBuf::from("examples/test.expr"));
                assert_eq!(output, Some(PathBuf::from("out.component.wasm")));
            }
            _ => panic!("expected wasm command"),
        }
    }

    #[test]
    fn cli_rejects_unknown_backend() {
        let err = parse_cli_args_from(["run", "examples/test.expr", "--backend", "nope"])
            .expect_err("cli parse should fail");
        assert!(err.contains("unknown backend: nope"));
    }

    #[test]
    fn cli_rejects_zero_arena() {
        let err = parse_cli_args_from(["run", "examples/test.expr", "--arena-mb", "0"])
            .expect_err("cli parse should fail");
        assert_eq!(err, "--arena-mb must be > 0");
    }

    #[test]
    fn cli_rejects_unknown_command() {
        let err =
            parse_cli_args_from(["nope", "examples/test.expr"]).expect_err("cli parse should fail");
        assert!(err.contains("unknown command: nope"));
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
    fn validate_cli_rejects_missing_llvm_backend() {
        let cli = parse_ok(&["build", "examples/test.expr", "--backend", "llvm"]);
        let err = validate_cli_runtime(&cli, false, true)
            .expect_err("llvm-less build should reject llvm backend");
        assert!(err.contains("llvm backend is not available"));
    }

    #[test]
    fn validate_cli_rejects_component_output_without_wasi_feature() {
        let cli = parse_ok(&["wasm", "component", "examples/test.expr"]);
        let err = validate_cli_runtime(&cli, true, false)
            .expect_err("component output should require wasi");
        assert_eq!(err, "component wasm output requires building with the `wasi` cargo feature");
    }

    #[test]
    fn validate_cli_accepts_component_output_when_available() {
        let cli = parse_ok(&["wasm", "component", "examples/test.expr"]);
        validate_cli_runtime(&cli, true, true).expect("component output should be accepted");
    }

    #[test]
    fn validate_cli_accepts_wasm_output_with_llvm_backend() {
        let cli = parse_ok(&["wasm", "core", "examples/test.expr"]);
        validate_cli_runtime(&cli, true, true).expect("wasm output should be accepted");
    }

    #[test]
    fn run_ir_executes_compiled_program() {
        let ir = "function %main() -> i64 system_v {\nblock0:\n    v0 = iconst.i64 7\n    return v0\n}\n";
        let result = run_ir(&ir).expect("run_ir should succeed");
        assert_eq!(result, Some(7));
    }

    #[test]
    fn handle_ir_writes_ir_file() {
        let unique = format!(
            "expr-compiler-main-test-{}.clif",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("system time should be after unix epoch")
                .as_nanos()
        );
        let output = std::env::temp_dir().join(unique);
        handle_ir(Path::new("input.expr"), Some(&output), false, "fn main() do\n    7\nend")
            .expect("ir writing should succeed");

        let ir = std::fs::read_to_string(&output).expect("ir output should exist");
        assert!(ir.contains("function"));

        let _ = std::fs::remove_file(output);
    }

    #[test]
    fn handle_ir_propagates_run_ir_errors() {
        let err = handle_ir(Path::new("input.expr"), None, true, "fn main() do\n    1 / 0\nend")
            .expect_err("run-ir should propagate traps as errors");
        assert!(!err.is_empty());
    }

    #[test]
    fn format_debug_types_includes_function_and_assignment_annotations() {
        let source =
            "fn main() do\n    ok, value, err = string_try_parse_bigint(\"12\")\n    value\nend\n";
        let module = Module::try_from_source(source).expect("source should parse");
        let user_functions =
            parse_user_functions_for_debug(source).expect("user functions should parse");
        let rendered = format_debug_types(source, &user_functions, &module)
            .expect("debug types should render");
        assert!(rendered.contains("#? returns bigint"));
        assert!(rendered.contains("#? ok: int, value: bigint, err: string"));
    }

    #[test]
    fn format_debug_types_includes_list_item_kinds() {
        let source = "fn main(args) do\n    xs = [1, \"a\"]\n    args\nend\n";
        let module = Module::try_from_source(source).expect("source should parse");
        let user_functions =
            parse_user_functions_for_debug(source).expect("user functions should parse");
        let rendered = format_debug_types(source, &user_functions, &module)
            .expect("debug types should render");
        assert!(rendered.contains("#? inputs [args: list<string>]; returns list<string>"));
        assert!(rendered.contains("#? xs: list<int | string>"));
    }

    #[test]
    fn format_debug_types_includes_map_value_kinds() {
        let source = "fn main() do\n    m = map_new()\n    map_set(m, \"count\", 1)\n    map_set(m, \"name\", \"x\")\n    m\nend\n";
        let module = Module::try_from_source(source).expect("source should parse");
        let user_functions =
            parse_user_functions_for_debug(source).expect("user functions should parse");
        let rendered = format_debug_types(source, &user_functions, &module)
            .expect("debug types should render");
        assert!(rendered.contains("#? returns map<string, int | string>"));
        assert!(rendered.contains("#? m: map<string, int | string>"));
    }

    #[test]
    fn format_source_preserves_python_style_blocks() {
        let source =
            "fn main():\n    value=1+2\n    if true:\n        value\n    else:\n        0\n";
        let formatted = format_source(source, &FormatConfig::inferred_from_source(source))
            .expect("source should format");
        assert_eq!(
            formatted,
            "fn main():\n    value = 1 + 2\n    if true:\n        value\n    else:\n        0\n"
        );
    }

    #[test]
    fn format_source_preserves_map_and_lambda_syntax() {
        let source = "fn main() do\n    ops={\"+\": fn lhs, rhs -> lhs+rhs end}\n    ops\nend\n";
        let formatted =
            format_source(source, &FormatConfig::default()).expect("source should format");
        assert_eq!(
            formatted,
            "fn main() do\n    ops = {\n        \"+\": fn lhs, rhs -> lhs + rhs end,\n    }\n    ops\nend\n"
        );
    }

    #[test]
    fn run_jit_executes_compiled_program() {
        let result = run_jit(
            Path::new("input.expr"),
            "fn main() do\n    1 + 2 * 3\nend",
            CodegenBackend::Cranelift,
            16,
            &[],
        )
        .expect("run_jit should succeed");
        assert_eq!(result, 7);
    }

    #[test]
    fn run_jit_passes_program_args() {
        let program_args = vec!["hello".to_string(), "world".to_string()];
        let result = run_jit(
            Path::new("input.expr"),
            "fn main(args) do\n    list_len(args) + bytes_len(list_get(args, 0))\nend",
            CodegenBackend::Cranelift,
            16,
            &program_args,
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
