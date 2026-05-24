use cranelift::{codegen::data_value::DataValue, interpreter::step::ControlFlow};
use expr_compiler::module::{CodegenBackend, Module, llvm_backend_available};
use expr_compiler::runtime::{
    build_argv_list_value, configure_runtime_arena, decode_int, reset_runtime_arena,
};
use pico_args::Arguments;
use std::path::{Path, PathBuf};

const USAGE: &str = "usage: expr-compiler <source-file> [-o <output>] [--emit-ir] [--run-ir] [--run-jit] [--backend <cranelift|llvm>] [--arena-mb <n>] [-- <arg>...]";

#[derive(Debug)]
struct CliArgs {
    input: PathBuf,
    output: Option<PathBuf>,
    emit_ir: bool,
    run_ir: bool,
    run_jit: bool,
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
            "component wasm output requires building with the `wasi` cargo feature".to_string(),
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

    let input = args
        .free_from_str::<String>()
        .map_err(|_| USAGE.to_string())?;

    let mut program_args = args
        .finish()
        .into_iter()
        .map(|x| x.to_string_lossy().to_string())
        .collect::<Vec<_>>();
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
        backend,
        arena_mb,
        program_args,
    })
}

fn main() {
    let cli = match parse_cli_args() {
        Ok(cli) => cli,
        Err(err) => {
            eprintln!("{err}");
            let code = if err == USAGE { 0 } else { 1 };
            std::process::exit(code);
        }
    };

    let input = Path::new(&cli.input);
    let source = std::fs::read_to_string(input).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", input.display());
        std::process::exit(1);
    });

    if let Err(err) = validate_cli_runtime(&cli, llvm_backend_available(), cfg!(feature = "wasi")) {
        eprintln!("{err}");
        std::process::exit(1);
    }

    if cli.emit_ir || cli.run_ir {
        let ir = Module::from_source(&source).compile_to_ir();

        if cli.emit_ir {
            if let Some(output) = cli.output.as_ref() {
                std::fs::write(output, &ir).unwrap_or_else(|e| {
                    eprintln!("error writing IR: {e}");
                    std::process::exit(1);
                });
            } else {
                print!("{ir}");
            }
        }

        if cli.run_ir {
            let functions = cranelift_reader::parse_functions(&ir).unwrap_or_else(|e| {
                eprintln!("error parsing IR: {e}");
                std::process::exit(1);
            });
            let mut function_store = cranelift::interpreter::environment::FunctionStore::default();
            let mut first_func = None;
            for func in functions.iter() {
                first_func = Some(func.name.to_string()); // last wins = main
                function_store.add(func.name.to_string(), func);
            }
            let state = cranelift::interpreter::interpreter::InterpreterState::default()
                .with_function_store(function_store);
            let mut interpreter = cranelift::interpreter::interpreter::Interpreter::new(state);
            if let Some(func_name) = first_func {
                match interpreter.call_by_name(&func_name, &[]) {
                    Ok(ControlFlow::Return(res)) => {
                        if let Some(DataValue::I64(x)) = res.first() {
                            if let Some(decoded) = decode_int(*x) {
                                println!("{decoded}");
                            } else {
                                println!("{x}");
                            }
                        }
                    }
                    Ok(ControlFlow::Trap(trap)) => {
                        eprintln!("trap: {trap:?}");
                        std::process::exit(1);
                    }
                    Err(e) => {
                        eprintln!("interpreter error: {e}");
                        std::process::exit(1);
                    }
                    _ => {}
                }
            }
        }

        if !cli.run_jit {
            return;
        }
    }

    if cli.run_jit {
        configure_runtime_arena(cli.arena_mb * 1024 * 1024);
        reset_runtime_arena();
        let module = Module::from_source(&source);
        let (func_name, func_arity) =
            if let Some(main) = module.functions.iter().find(|func| func.name == "main") {
                (main.name.clone(), main.inputs.len())
            } else {
                let func = module.functions.first().unwrap_or_else(|| {
                    eprintln!("no functions found");
                    std::process::exit(1);
                });
                (func.name.clone(), func.inputs.len())
            };
        if func_arity > 1 {
            eprintln!("jit entry function supports at most one argument");
            std::process::exit(1);
        }
        let jit = module.compile_to_jit_with_backend(cli.backend);
        let int_result = if let Some(ptr) = jit.get_int_result_fn_ptr(&func_name) {
            if func_arity == 1 {
                let (arg_tag, arg_payload) = build_argv_list_value(&cli.program_args);
                let func = unsafe {
                    std::mem::transmute::<*const u8, extern "C" fn(i64, i64) -> i64>(ptr)
                };
                func(arg_tag, arg_payload)
            } else {
                let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
                func()
            }
        } else {
            let ptr = jit.get_fn_ptr(&func_name);
            let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
            let result = func();
            decode_int(result).unwrap_or_else(|| {
                eprintln!("runtime error: main returned non-integer value");
                std::process::exit(1);
            })
        };
        std::process::exit(int_result as i32);
    }

    let output = finalize_output_path(
        cli.output
            .unwrap_or_else(|| input.with_extension("").to_path_buf()),
    );

    Module::from_source(&source).compile_to_executable_with_backend(&output, cli.backend);
    println!("compiled to {}", output.display());
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
        assert_eq!(
            finalize_output_path(PathBuf::from("demo.bin")),
            PathBuf::from("demo.bin")
        );
    }

    #[test]
    fn classify_output_detects_native() {
        assert_eq!(classify_output(None), OutputKind::Native);
        assert_eq!(
            classify_output(Some(Path::new("examples/out"))),
            OutputKind::Native
        );
    }

    #[test]
    fn classify_output_detects_wasm() {
        assert_eq!(
            classify_output(Some(Path::new("examples/out.wasm"))),
            OutputKind::Wasm
        );
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
        let cli = parse_ok(&[
            "examples/test.expr",
            "--backend",
            "llvm",
            "-o",
            "out.component.wasm",
        ]);
        let err = validate_cli_runtime(&cli, true, false)
            .expect_err("component output should require wasi");
        assert_eq!(
            err,
            "component wasm output requires building with the `wasi` cargo feature"
        );
    }

    #[test]
    fn validate_cli_accepts_component_output_when_available() {
        let cli = parse_ok(&[
            "examples/test.expr",
            "--backend",
            "llvm",
            "-o",
            "out.component.wasm",
        ]);
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
}
