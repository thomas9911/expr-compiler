use expr_compiler::module::Module;
use std::path::Path;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: {} <source-file> [-o <output>]", args[0]);
        std::process::exit(1);
    }

    let input = Path::new(&args[1]);
    let source = std::fs::read_to_string(input).unwrap_or_else(|e| {
        eprintln!("error reading {}: {e}", input.display());
        std::process::exit(1);
    });

    let output = if let Some(pos) = args.iter().position(|a| a == "-o") {
        Path::new(&args[pos + 1]).to_path_buf()
    } else {
        input.with_extension("")
    };

    Module::from_source(&source).compile_to_executable(&output);
    println!("compiled to {}", output.display());
}
