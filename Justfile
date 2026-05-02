examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file" --run-jit; done

llvm-test:
    bash scripts/llvm-backend.sh test

llvm-build:
    bash scripts/llvm-backend.sh build

llvm-run input:
    bash scripts/llvm-backend.sh run --input {{input}}

llvm-example name:
    bash scripts/llvm-backend.sh example --input {{name}}

compile-examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file"; done

compile-llvm-examples:
    for file in examples/*.expr; do echo "$file"; bash scripts/llvm-backend.sh compile --input "$file"; done

run-examples:
    for file in examples/*.exe; do \
        if [ -f "$file" ]; then echo "$file"; "$file"; fi; \
    done
    for file in examples/*; do \
        if [ -f "$file" ] && [ -x "$file" ] && [ "${file##*.}" = "${file}" ] && [ "${file##*/}" != "README" ]; then \
            echo "$file"; "$file"; \
        fi; \
    done

run-llvm-examples:
    for file in examples/*.expr; do echo "$file"; bash scripts/llvm-backend.sh run --input "$file"; done

clean-examples:
    rm -f examples/*.exe
    for file in examples/*; do \
        if [ -f "$file" ] && [ -x "$file" ] && [ "${file##*.}" = "${file}" ] && [ "${file##*/}" != "README" ]; then \
            rm -f "$file"; \
        fi; \
    done
