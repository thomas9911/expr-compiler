use cranelift::{codegen::data_value::DataValue, interpreter::step::ControlFlow};
use expr_compiler::module::Module;
use std::path::Path;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} <source-file> [-o <output>] [--emit-ir]", args[0]);
        std::process::exit(1);
    }

    let input = Path::new(&args[1]);
    let source = std::fs::read_to_string(input).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", input.display());
        std::process::exit(1);
    });

    let emit_ir = args.contains(&"--emit-ir".to_string());
    let run_ir = args.contains(&"--run-ir".to_string());

    if emit_ir || run_ir {
        let ir = Module::from_source(&source).compile_to_ir();

        if emit_ir {
            if let Some(pos) = args.iter().position(|a| a == "-o") {
                std::fs::write(&args[pos + 1], &ir).unwrap_or_else(|e| {
                    eprintln!("error writing IR: {e}");
                    std::process::exit(1);
                });
            } else {
                print!("{ir}");
            }
        }
        if run_ir {
            let functions = cranelift_reader::parse_functions(&ir).unwrap_or_else(|e| {
                eprintln!("error reading IR: {e}");
                std::process::exit(1);
            });
            let mut function_store = cranelift::interpreter::environment::FunctionStore::default();
            let mut first_func = None;
            for func in functions.iter() {
                first_func = Some(func.name.to_string());
                function_store.add(func.name.to_string(), func);
            }
            let state = cranelift::interpreter::interpreter::InterpreterState::default();
            let state = state.with_function_store(function_store);
            let mut interpreter = cranelift::interpreter::interpreter::Interpreter::new(state);
            if let Some(func_name) = first_func {
                if let Ok(ControlFlow::Return(res)) = interpreter.call_by_name(&func_name, &[]) {
                    if let DataValue::I64(x) = &res[0] {
                        println!("{}", x);
                    }
                }
            }
        }
        return;
    }

    let output = if let Some(pos) = args.iter().position(|a| a == "-o") {
        Path::new(&args[pos + 1]).to_path_buf()
    } else {
        input.with_extension("")
    };

    Module::from_source(&source).compile_to_executable(&output);
    println!("compiled to {}", output.display());
}
