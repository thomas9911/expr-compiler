# another programming language (or expression thing)

Concept, have a interpreter and a 'compiled' output.

## LLVM backend

The repo has an optional LLVM backend behind the Cargo feature `llvm-backend`.

- Default backend: `cranelift`
- LLVM backend selector: `--backend llvm`
- LLVM modes currently used in this repo:
  - JIT: `--run-jit --backend llvm`
  - Native compile: `--backend llvm`

### Bash helper

Use the helper script to build, test, run, or compile against a local LLVM 20 build:

```bash
bash scripts/llvm-backend.sh test
bash scripts/llvm-backend.sh build
bash scripts/llvm-backend.sh example --input fib
bash scripts/llvm-backend.sh run --input examples/fib.expr
bash scripts/llvm-backend.sh compile --input examples/fib.expr
```

Equivalent `just` commands:

```text
just llvm-test
just llvm-build
just llvm-example fib
just llvm-run examples/fib.expr
just compile-llvm-examples
just run-llvm-examples
just check-matrix
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
