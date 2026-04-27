examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file" --run-jit; done

compile-examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file"; done

run-examples:
    for file in examples/*.exe; do echo "$file"; "$file"; done

clean-examples:
    rm examples/*.exe
