# AGENTS.md

Guidance for coding agents working in this repository.

## Project Summary

- Language/compiler project in Rust.
- Supports:
  - Interpreter path (`src/interpreter.rs`) for tests/experiments.
  - Cranelift JIT path (`--run-jit`).
  - Native executable path (object + link step).
- Value model is pair-valued internally: `(tag, payload)`.
- Lists store inline `Value` elements, not boxed integer handles.
- Function values exist as tagged values and are currently used by higher-order list builtins.
- Runtime behavior is split between:
  - IR-defined runtime builtins in `src/module/runtime_ir.rs`.
  - Host runtime helpers in `src/runtime.rs`.
  - Platform wrappers in `src/wrapper/`.

## Important Files

- `src/module.rs`: compile pipeline (JIT, IR, object, executable), AST lowering.
- `src/module/runtime_ir.rs`: IR runtime builtins (`__rt_*`), list/int operations, allocator wiring.
- `src/runtime.rs`: host arena helpers, compatibility boxing helpers, print/list-print host functions.
- `src/wrapper/windows.rs`: Windows executable wrapper symbols.
- `src/wrapper/unix.rs`: Unix executable wrapper symbols for LLVM/native wrapper flow.
- `src/wrapper/unix.c`: small Unix C wrapper used by the Cranelift native path.
- `examples/*.expr`: language examples.
- `Justfile`: helper commands.

## Current Language Features

- BigInt values are supported:
  - `123n`
  - `bigint_from_int(x)`
  - `bigint_compare(a, b)`
  - `bigint_add(a, b)`
  - `bigint_subtract(a, b)`
  - `bigint_multiply(a, b)`
  - `bigint_divide(a, b)`
  - `bigint_modulo(a, b)`
  - `a + b`, `a - b`, `a * b`, `a / b`, and `a % b` when both sides are `BigInt`
  - `a == b`, `a != b`, `a < b`, `a <= b`, `a > b`, and `a >= b` when both sides are `BigInt`
  - mixed `Int` / `BigInt` operator arithmetic and comparisons promote the `Int` operand
  - explicit `bigint_*` builtins also promote `Int` arguments to `BigInt`
- Anonymous functions support captures:
  - `fn item -> item * factor end`
- Higher-order list builtins are available:
  - `list_map(xs, callback)`
  - `list_filter(xs, callback)`
- Callbacks must currently have arity `1`.
- Top-level named functions can be used as function values in expression position.
- Function values can be stored in variables and passed around.
- Generic direct function-value calls are supported:
  - `f(10)`

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
- `just compile-wasm-examples`
  - Compiles each `examples/*.expr` to a core LLVM `.wasm`.
- `just compile-component-examples`
  - Compiles each `examples/*.expr` to a LLVM `wasi:cli/command` component (`*.component.wasm`).
  - Requires the Cargo feature `wasi`.
- `just check-matrix`
  - Runs `scripts/check-matrix.py --release`.
  - Uses Cranelift JIT as the baseline and compares Cranelift/LLVM runnable modes, including LLVM Wasm and LLVM `wasi:cli/command` components.
  - The baseline defines expected stdout and success/failure class.
  - Expected-failure examples are allowed; exact non-zero exit codes do not need to match across native, Wasm, and component runners.
  - Excludes `run-ir`, because that path does not support the same stdout behavior.
  - Fails native-mode checks if a compiled executable exceeds `50 KB`.
- `just run-examples`
  - Runs compiled example binaries on both platforms:
  - Windows: `examples/*.exe`
  - Linux/Unix: executable files in `examples/` without an extension
- `just run-llvm-examples`
  - Runs each `examples/*.expr` through LLVM JIT via `scripts/llvm-backend.sh`.
- `just run-wasm <file>`
  - Runs a core `.wasm` through `scripts/run-wasm.js`.
- `just run-component <file>`
  - Runs a `wasi:cli/command` component with `wasmtime run`.
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
- Cranelift and LLVM both use pair-valued internal execution and inline list element storage.
- Cranelift and LLVM both lower function values as closure objects:
  - `TAG_FUNCTION`
  - payload = pointer to `{ function_ordinal, env_ptr }`
- BigInt values use `TAG_BIGINT` and a dedicated heap object:
  - `{ sign, len, cap, ptr }`
  - limbs are `u32`
- BigInt arithmetic and comparisons are implemented in backend IR, not in Rust runtime arithmetic helpers.
- Print/list_print are still host-runtime boundaries.
- LLVM core Wasm keeps print/list_print as custom imports for the Node runner.
- LLVM component output is behind the Cargo feature `wasi`.
- LLVM component output builds a Preview 1-style core command module and then adapts it to a Preview 2 `wasi:cli/command` component using the embedded adapter from `wasi-preview1-component-adapter-provider`.
- Arena exhaustion is expected to report explicitly as:
  - `runtime error: out of arena memory`
- `__expr_value_int_host` is now a compatibility/test helper, not part of the main internal execution model.

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
