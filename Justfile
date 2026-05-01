examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file" --run-jit; done

llvm-test:
    pwsh -File scripts/llvm-backend.ps1 test

llvm-build:
    pwsh -File scripts/llvm-backend.ps1 build

llvm-run input:
    pwsh -File scripts/llvm-backend.ps1 run -Input {{input}}

llvm-example name:
    pwsh -File scripts/llvm-backend.ps1 example -Input {{name}}

compile-examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file"; done

run-examples:
    for file in examples/*.exe; do \
        if [ -f "$file" ]; then echo "$file"; "$file"; fi; \
    done
    for file in examples/*; do \
        if [ -f "$file" ] && [ -x "$file" ] && [ "${file##*.}" = "${file}" ] && [ "${file##*/}" != "README" ]; then \
            echo "$file"; "$file"; \
        fi; \
    done

clean-examples:
    rm -f examples/*.exe
    for file in examples/*; do \
        if [ -f "$file" ] && [ -x "$file" ] && [ "${file##*.}" = "${file}" ] && [ "${file##*/}" != "README" ]; then \
            rm -f "$file"; \
        fi; \
    done
