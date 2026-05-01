# another programming language (or expression thing)

Concept, have a interpreter and a 'compiled' output.

## LLVM backend

The repo now has an optional LLVM JIT backend behind the Cargo feature `llvm-backend`.

- Default backend: `cranelift`
- LLVM backend selector: `--backend llvm`
- Current LLVM support: JIT only, so use `--run-jit`

### PowerShell helper

Use the helper script to build or test the LLVM backend against a local LLVM 20 build:

```powershell
pwsh -File scripts/llvm-backend.ps1 test
pwsh -File scripts/llvm-backend.ps1 build
pwsh -File scripts/llvm-backend.ps1 example -Input fib
pwsh -File scripts/llvm-backend.ps1 run -Input examples/fib.expr
```

Equivalent `just` commands:

```text
just llvm-test
just llvm-build
just llvm-example fib
just llvm-run examples/fib.expr
```

### LLVM requirements

- Inkwell version: `0.9.0`
- LLVM feature: `llvm20-1`
- Rust host and LLVM host must match exactly
- On Windows with this repo, that means `x86_64-pc-windows-msvc`

The helper script will:

- set `LLVM_SYS_201_PREFIX`
- set `LLVM_CONFIG_PATH`
- add the LLVM `bin` directory to `PATH`
- use `target_llvm_backend/` as a separate Cargo target dir
- fail fast if `rustc` and `llvm-config --host-target` do not match

You can override the LLVM path with either:

- `-LlvmRoot <path>`
- `LLVM_SYS_201_PREFIX`

If both are unset, the script defaults to:

```text
F:\Rust\llvm-project-20.1.8.src\llvm-project-20.1.8.src\build
```
