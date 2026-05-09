# another programming language (or expression thing)

Concept, have an interpreter and compiled output.

## Runtime model

The current runtime value model is:

- internal compiled values are carried as `(tag, payload)` pairs
- list elements are stored inline as full `Value` records
- host printing is still a runtime boundary, but normal computation is pair-native

Current tag usage:

- `Int`
- `List`
- `String` is reserved in the value model, but not implemented as a language feature yet

## LLVM backend

The repo has an optional LLVM backend behind the Cargo feature `llvm-backend`.

- Default backend: `cranelift`
- LLVM backend selector: `--backend llvm`
- LLVM modes currently used in this repo:
  - JIT: `--run-jit --backend llvm`
  - Native compile: `--backend llvm`
  - Core Wasm module: `--backend llvm -o out.wasm`
  - WASI Preview 2 command component: `--features llvm-backend,wasi --backend llvm -o out.component.wasm`

### Bash helper

Use the helper script to build, test, run, or compile against a local LLVM 20 build:

```bash
bash scripts/llvm-backend.sh test
bash scripts/llvm-backend.sh build
bash scripts/llvm-backend.sh example --input fib
bash scripts/llvm-backend.sh run --input examples/fib.expr
bash scripts/llvm-backend.sh compile --input examples/fib.expr

### LLVM core Wasm output

You can emit a core WebAssembly module with the LLVM backend by choosing a
`.wasm` output path:

```bash
cargo run --release --features llvm-backend -- examples/fib.expr --backend llvm -o fib.wasm
```

Current behavior:

- emits a core Wasm module targeting `wasm32-unknown-unknown`
- exports `__expr_main_i64`
- exports linear memory
- leaves print functions as imports:
  - `__expr_wasm_print_host`
  - `__expr_wasm_list_print_host`

### WASI Preview 2 command component output

You can emit a runnable `wasi:cli/command` component with the LLVM backend by
choosing a `.component.wasm` output path:

```bash
cargo run --release --features llvm-backend,wasi -- examples/fib.expr --backend llvm -o fib.component.wasm
```

Current behavior:

- first builds a Preview 1-style core Wasm command module
- then wraps it into a Preview 2 component in-process via `wit-component`
- exports a runnable `wasi:cli/command` component intended for:
  - `wasmtime run fib.component.wasm`

Additional tooling requirements:

- the Cargo feature `wasi` must be enabled
- `wasmtime` is needed if you want to run the resulting component locally

The Preview 1 command adapter is embedded through the Rust crate
`wasi-preview1-component-adapter-provider`, so no external adapter file is needed.

Run with Wasmtime:

```bash
wasmtime run fib.component.wasm
```

Tooling requirements:

- `wasm-ld` must be available
- lookup order is:
  - `WASM_LD`
  - `LLVM_SYS_201_PREFIX/bin/wasm-ld`
  - `wasm-ld` from `PATH`

To run a generated module under Node.js:

```bash
node scripts/run-wasm.js fib.wasm
```

Optional export override:

```bash
node scripts/run-wasm.js fib.wasm --export __expr_main_i64
```

Equivalent `just` commands:

```text
just llvm-test
just llvm-build
just llvm-example fib
just llvm-run examples/fib.expr
just compile-llvm-examples
just compile-wasm-examples
just compile-component-examples
just run-wasm fib.wasm
just run-component fib.component.wasm
just run-llvm-examples
just check-matrix
```

`just check-matrix` includes:

- LLVM core Wasm mode, run through `scripts/run-wasm.js`
- LLVM `wasi:cli/command` component mode, run through `wasmtime run`

If your non-interactive shell does not expose the JavaScript runtime in `PATH`,
you can override it explicitly:

```bash
JS_RUNTIME=node just check-matrix
```

or on systems where the executable is named differently:

```bash
JS_RUNTIME=nodejs just check-matrix
```

If `wasmtime` is not in `PATH`, you can override that explicitly too:

```bash
WASMTIME=/path/to/wasmtime just check-matrix
```

### Local test script

If you want one command that checks both your Windows and WSL setup, create a local
`local_test.sh` in the repo root and keep your machine-specific LLVM paths there.

Example:

```bash
#! /bin/env bash

set -Eeuo pipefail

windows_llvm_root='<your-path>/llvm-project-20.1.8.src/build'
wsl_llvm_root='<your-path>/llvm-project-20.1.8.src/build'

echo "testing windows"
LLVM_SYS_201_PREFIX="$windows_llvm_root" just check-matrix

echo "testing linux through wsl"
wsl -e bash -lc 'cd /mnt/f/Rust/expr-compiler && LLVM_SYS_201_PREFIX='"'"$wsl_llvm_root"'"' just check-matrix'
```

Notes:

- adjust `windows_llvm_root` and `wsl_llvm_root` for your machine
- `local_test.sh` is git-ignored, so you can customize it freely
- the committed scripts no longer contain hardcoded local LLVM paths

### LLVM requirements

- Inkwell version: `0.9.0`
- LLVM feature: `llvm20-1`
- Rust host and LLVM host must match exactly
- Examples:
  - Windows: `x86_64-pc-windows-msvc`
  - WSL/Linux: `x86_64-unknown-linux-gnu`

The helper script will:

- set `LLVM_SYS_201_PREFIX`
- set `LLVM_CONFIG_PATH`
- add the LLVM `bin` directory to `PATH`
- use `target_llvm_backend/` as a separate Cargo target dir
- fail fast if `rustc` and `llvm-config --host-target` do not match

You can override the LLVM path with either:

- `--llvm-root <path>`
- `LLVM_SYS_201_PREFIX`

If both are unset, the helper scripts now fail and ask you to configure the path explicitly.
