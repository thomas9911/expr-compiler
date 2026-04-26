examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file" --run-jit; done

compile-examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file"; done

clean-examples:
    rm examples/*.exe
