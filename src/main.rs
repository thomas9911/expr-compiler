use cranelift::{codegen::data_value::DataValue, interpreter::step::ControlFlow};
use expr_compiler::module::Module;
use expr_compiler::runtime::{configure_runtime_arena, decode_int, reset_runtime_arena};
use pico_args::Arguments;
use std::path::{Path, PathBuf};

const USAGE: &str = "usage: expr-compiler <source-file> [-o <output>] [--emit-ir] [--run-ir] [--run-jit] [--arena-mb <n>]";

struct CliArgs {
    input: PathBuf,
    output: Option<PathBuf>,
    emit_ir: bool,
    run_ir: bool,
    run_jit: bool,
    arena_mb: usize,
}

fn finalize_output_path(mut output: PathBuf) -> PathBuf {
    if cfg!(windows) && output.extension().is_none() {
        output.set_extension("exe");
    }
    output
}

fn parse_cli_args() -> Result<CliArgs, String> {
    let mut args = Arguments::from_env();

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

    let remaining = args.finish();
    if !remaining.is_empty() {
        let unknown = remaining
            .into_iter()
            .map(|x| x.to_string_lossy().to_string())
            .collect::<Vec<_>>()
            .join(" ");
        return Err(format!("unknown arguments: {unknown}\n{USAGE}"));
    }

    Ok(CliArgs {
        input: PathBuf::from(input),
        output,
        emit_ir,
        run_ir,
        run_jit,
        arena_mb,
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
        let jit = Module::from_source(&source).compile_to_jit();
        let func_name = if jit.has_function("main") {
            "main"
        } else {
            jit.user_function_names().next().unwrap_or_else(|| {
                eprintln!("no functions found");
                std::process::exit(1);
            })
        };
        let ptr = jit.get_fn_ptr(func_name);
        let func = unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> i64>(ptr) };
        let result = func();
        let int_result = decode_int(result).unwrap_or_else(|| {
            eprintln!("runtime error: main returned non-integer value");
            std::process::exit(1);
        });
        std::process::exit(int_result as i32);
    }

    let output = finalize_output_path(
        cli.output
            .unwrap_or_else(|| input.with_extension("").to_path_buf()),
    );

    Module::from_source(&source).compile_to_executable(&output);
    println!("compiled to {}", output.display());
}
