# AGENTS.md

Guidance for coding agents working in this repository.

## Project Summary

- Language/compiler project in Rust.
- Supports:
  - Interpreter path (`src/interpreter.rs`) for tests/experiments.
  - Cranelift JIT path (`--run-jit`).
  - Native executable path (object + link step).
- Value model is boxed/tagged (`Int`, `List`).
- Runtime behavior is split between:
  - IR-defined runtime builtins in `src/module/runtime_ir.rs`.
  - Host runtime helpers in `src/runtime.rs`.
  - Platform wrappers in `src/wrapper/`.

## Important Files

- `src/module.rs`: compile pipeline (JIT, IR, object, executable), AST lowering.
- `src/module/runtime_ir.rs`: IR runtime builtins (`__rt_*`), list/int operations, allocator wiring.
- `src/runtime.rs`: host arena helpers, decode helpers, print/list-print host functions.
- `src/wrapper/windows.rs`: Windows executable wrapper symbols.
- `src/wrapper/unix.c`: Unix executable wrapper symbols.
- `examples/*.expr`: language examples.
- `Justfile`: helper commands.

## Build and Test

- Run tests:
  - `cargo test -q`
- Run one example with JIT:
  - `cargo run --release -q -- examples/<name>.expr --run-jit`
- Build native executable from source file:
  - `cargo run --release -q -- examples/<name>.expr`

## Just Commands

From `Justfile`:

- `just examples`
  - Runs all `examples/*.expr` with JIT:
  - `cargo run --release -q -- "$file" --run-jit`
- `just compile-examples`
  - Compiles each `examples/*.expr` to a native executable.
- `just compile-llvm-examples`
  - Compiles each `examples/*.expr` through `scripts/llvm-backend.sh`.
- `just check-matrix`
  - Runs `scripts/check-matrix.py --release`.
  - Uses Cranelift JIT as the baseline and compares Cranelift/LLVM runnable modes.
  - Excludes `run-ir`, because that path does not support the same stdout behavior.
  - Fails native-mode checks if a compiled executable exceeds `50 KB`.
- `just run-examples`
  - Runs compiled example binaries on both platforms:
  - Windows: `examples/*.exe`
  - Linux/Unix: executable files in `examples/` without an extension
- `just run-llvm-examples`
  - Runs each `examples/*.expr` through LLVM JIT via `scripts/llvm-backend.sh`.
- `just clean-examples`
  - Removes compiled example binaries on both platforms:
  - Windows: `examples/*.exe`
  - Linux/Unix: executable files in `examples/` without an extension

## Current Runtime Architecture (Do Not Break)

- `setup_builtins` (non-JIT) uses module data-backed arena (`__rt_arena`, `__rt_arena_offset`).
- `setup_builtins_jit` uses host-address-backed allocator for JIT stability on Linux/WSL.
- JIT print/list_print go through local IR shims with `call_indirect` to host addresses.
- Runtime memcpy is IR-defined (`__rt_memcpy`) to avoid external relocation issues.
- Arena data for native/object path is zero-init (`.bss`), not embedded initialized bytes.

## Platform Notes

- Windows executable path uses `src/wrapper/windows.rs`.
- Unix executable path uses `src/wrapper/unix.c`.
- If JIT fails on Linux with relocation range panics, check for accidental reintroduction of external direct calls or data relocations in JIT runtime path.

## Change Rules

- Keep language semantics identical across JIT and native paths.
- Prefer sharing logic between JIT and non-JIT runtime builders; only keep wiring differences where required.
- Preserve overflow/underflow traps for integer ops.
- Preserve list bounds/empty checks as traps.
- Do not revert unrelated working-tree changes.

## Validation Checklist for Runtime/Codegen Changes

1. `cargo test -q` passes.
2. A simple arithmetic example runs with `--run-jit`.
3. A list example runs with `--run-jit`.
4. Native compile path still links and runs on the target platform.
