examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file" --run-jit; done

llvm-test:
    bash scripts/llvm-backend.sh test

llvm-build:
    bash scripts/llvm-backend.sh build

llvm-run input:
    bash scripts/llvm-backend.sh run --input {{ input }}

llvm-example name:
    bash scripts/llvm-backend.sh example --input {{ name }}

check-matrix:
    if command -v python >/dev/null 2>&1; then python scripts/check-matrix.py --release; else python3 scripts/check-matrix.py --release; fi

crap:
    cargo llvm-cov --all-features --lcov --output-path lcov.info
    if command -v python >/dev/null 2>&1; then python scripts/normalize-lcov.py lcov.info lcov.relative.info; else python3 scripts/normalize-lcov.py lcov.info lcov.relative.info; fi
    cargo crap --lcov lcov.relative.info --exclude 'src/wrapper/*.rs' --exclude 'out/**' --top 20 --format json

compile-examples:
    for file in examples/*.expr; do echo "$file"; cargo run --release -q -- "$file"; done

compile-llvm-examples:
    for file in examples/*.expr; do echo "$file"; bash scripts/llvm-backend.sh compile --input "$file"; done

compile-wasm-examples:
    for file in examples/*.expr; do \
        echo "$file"; \
        cargo run --release --features llvm-backend -- "$file" --backend llvm -o "${file%.expr}.wasm"; \
    done

compile-component-examples:
    for file in examples/*.expr; do \
        echo "$file"; \
        cargo run --release --features llvm-backend,wasi -- "$file" --backend llvm -o "${file%.expr}.component.wasm"; \
    done

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

run-wasm file:
    node scripts/run-wasm.js {{ file }}

run-wasm-examples:
    for file in examples/*.wasm; do \
        if [ -f "$file" ] && [ "${file%.component.wasm}" = "$file" ]; then \
            echo "$file"; \
            node scripts/run-wasm.js "$file"; \
        fi; \
    done

run-component file:
    wasmtime run {{ file }}

run-component-examples:
    for file in examples/*.component.wasm; do \
        if [ -f "$file" ]; then \
            echo "$file"; \
            wasmtime run "$file"; \
        fi; \
    done

clean-examples:
    rm -f examples/*.exe
    rm -f examples/*.wasm
    for file in examples/*; do \
        if [ -f "$file" ] && [ -x "$file" ] && [ "${file##*.}" = "${file}" ] && [ "${file##*/}" != "README" ]; then \
            rm -f "$file"; \
        fi; \
    done
