# AGENTS.md

Guidance for coding agents working in this repository.

## Project Summary

- Language/compiler project in Rust.
- Supports:
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
- `src/wrapper/unix.rs`: Unix executable wrapper symbols for native executable output.
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
  - `bigint_bitand(a, b)`
  - `bigint_bitor(a, b)`
  - `bigint_bitxor(a, b)`
  - `bigint_shl(a, shift)`
  - `bigint_shr(a, shift)`
  - `a + b`, `a - b`, `a * b`, `a / b`, and `a % b` when both sides are `BigInt`
  - `a & b`, `a | b`, `a ^ b`, `a << b`, and `a >> b` when the operands are supported bigint bitwise forms
  - `a == b`, `a != b`, `a < b`, `a <= b`, `a > b`, and `a >= b` when both sides are `BigInt`
  - mixed `Int` / `BigInt` operator arithmetic and comparisons promote the `Int` operand
  - explicit `bigint_*` builtins also promote `Int` arguments to `BigInt`
  - bigint bitwise support currently targets non-negative values first:
    - `&`, `|`, and `^` accept `Int`, `BigInt`, or mixed `Int` / `BigInt` operands by promoting `Int`
    - `<<` and `>>` accept a bigint-or-int left operand and an `Int` shift count
    - negative bigint operands in bitwise operations trap at runtime
    - negative shift counts or shift counts `>= 64` still trap
- String values are supported:
  - `"hello"`
  - `print("hello")`
  - `string_concat("ab", "cd")`
  - `bytes_len("hello")`
  - `bytes_get("hello", 1)`
  - `bytes_slice("hello", 1, 4)`
  - `bytes_pop(s)`
  - `bytes_insert(s, 1, 97)`
  - `bytes_remove(s, 2)`
  - `bytes_push(s, 33)`
  - `bytes_set(s, 0, 72)`
  - `string_chars(s)`
  - `string_iter_done(it)`
  - `string_iter_next(it)`
  - `string_copy(s)`
  - `string_is_empty(s)`
  - `string_is_not_empty(s)`
  - `string_len(s)`
  - `string_first(s)`
  - `string_last(s)`
  - `string_try_first(s)`
  - `string_try_last(s)`
  - `string_starts_with(s, prefix)`
  - `string_ends_with(s, suffix)`
  - `string_contains(s, needle)`
  - `bytes_try_get(s, index)`
  - `string_try_pop(s)`
  - `string_is_ascii(s)`
  - `string_all(s, predicate)`
  - `string_is_integer(s)`
  - `string_try_parse_integer(s)`
  - `string_try_parse_bigint(s)`
  - `integer_to_string(x)`
  - `string_from_codepoints(xs)`
  - `type_of(value)`
  - `is_int(value)`
  - `is_bigint(value)`
  - `is_string(value)`
  - `is_list(value)`
  - `is_map(value)`
  - `is_map_iter(value)`
  - `is_function(value)`
  - `is_string_iter(value)`
  - `map_new()`
  - `map_set(m, key, value)`
  - `map_len(m)`
  - `map_has(m, key)`
  - `map_get(m, key)`
  - `map_delete(m, key)`
  - `map_iter(m)`
  - `map_iter_done(it)`
  - `map_iter_next(it)`
  - `map_iter_key(it)`
  - `map_iter_value(it)`
  - `map_iter_advance(it)`
  - `map_keys(m)`
  - `map_values(m)`
  - `map_try_get(m, key)`
  - `map_try_delete(m, key)`
  - `map_try_pop(m)`
  - `map_update(m, key, callback)`
  - `map_update_or_default(m, key, default_value, callback)`
  - `string_repeat(s, n)`
  - `string_reverse(s)`
  - `"a" == "a"`
  - `"a" != "b"`
  - string literals support `\"`, `\\`, `\n`, `\r`, and `\t`
  - `string_concat(a, b)` returns a fresh string
  - `bytes_len(s)` returns byte length
  - `bytes_get(s, i)` returns an `Int` byte value
  - `bytes_slice(s, start, end)` returns a new string over the byte range `[start, end)`
  - `bytes_pop(s)` mutates the string by shrinking it and returns the removed byte as an `Int`
  - `bytes_insert(s, index, byte)` mutates the string by inserting one byte in place
  - `bytes_remove(s, index)` mutates the string by removing one byte and returns that byte as an `Int`
  - `bytes_push(s, byte)` mutates the string by appending one byte
  - `bytes_set(s, index, byte)` mutates one byte in place
  - `string_chars(s)` creates a left-to-right UTF-8 code point iterator
  - `string_iter_done(it)` reports whether the iterator is exhausted
  - `string_iter_next(it)` returns the next Unicode scalar value as an `Int`
  - `string_copy(s)` returns a fresh independent string copy
  - some higher-level helpers can be autoloaded from compiler-managed `.expr` stdlib source instead of being backend builtins
  - current autoloaded helpers include `string_is_empty(s)`, `string_is_not_empty(s)`, `string_len(s)`, `string_first(s)`, `string_last(s)`, `string_try_first(s)`, `string_try_last(s)`, `string_starts_with(s, prefix)`, `string_ends_with(s, suffix)`, `string_contains(s, needle)`, `bytes_try_get(s, index)`, `string_try_pop(s)`, `string_is_ascii(s)`, `string_all(s, predicate)`, `string_is_integer(s)`, `string_repeat(s, n)`, and `string_reverse(s)`
  - `string_try_parse_integer(s)` returns `(ok, value, err)` where `ok` is a boolean alias, `value` is the parsed `Int` or `0`, and `err` is `""` on success or a short message on failure
  - `string_try_parse_bigint(s)` returns `(ok, value, err)` where `value` is the parsed `BigInt` or `bigint_from_int(0)`
  - `integer_to_string(x)` converts an `Int` to its decimal string form
  - `string_from_codepoints(xs)` takes a `list<int>` of Unicode scalar values and returns a UTF-8 string
  - `string_try_first(s)`, `string_try_last(s)`, `bytes_try_get(s, index)`, and `string_try_pop(s)` also return `(ok, value, err)` instead of trapping on empty strings or out-of-bounds access
  - `map_try_get(m, key)` returns `(ok, value, err)` where `value` is the stored value or `0`, and `err` is `""` on success or `"missing key"` on failure
  - `map_try_delete(m, key)` returns `(ok, value, err)` with the same failure contract and removes the entry on success
  - `map_try_pop(m)` returns `(ok, key, value)` and removes an arbitrary entry on success; on an empty map it returns `(false, "", 0)`
  - `map_update(m, key, callback)` updates an existing entry with `callback(value)` and returns normalized integer boolean success
  - `map_update_or_default(m, key, default_value, callback)` stores and returns `callback(current_or_default_value)`
  - `map_iter(m)` returns a map iterator over the current entries
  - `map_iter_done(it)` reports whether the iterator is exhausted
  - `map_iter_next(it)` returns `(key, value)` and advances the iterator
  - `map_iter_key(it)` returns the current key
  - `map_iter_value(it)` returns the current value
  - `map_iter_advance(it)` advances to the next occupied entry
  - `map_keys(m)` and `map_values(m)` are collection helpers built on that iterator surface
  - `type_of(value)` returns a stable debuggable type name such as `"int"`, `"bigint"`, `"string"`, `"list"`, `"map"`, `"map_iter"`, `"function"`, or `"string_iter"`
  - `is_int(value)`, `is_bigint(value)`, `is_string(value)`, `is_list(value)`, `is_map(value)`, `is_map_iter(value)`, `is_function(value)`, and `is_string_iter(value)` return normalized integer booleans based on the runtime tag
  - the compiler now performs conservative compile-time kind checks for obvious builtin and indexing misuse
  - it rejects only when the inferred kinds are confident; unknown values still rely on runtime checks
  - UTF-8 iterator operations validate encoding and trap on invalid byte sequences
  - string equality compares byte contents
  - `String == non-String` is false
  - `String != non-String` is true
- Anonymous functions support captures:
  - `fn item -> item * factor end`
- logical infix operators are supported:
  - `a and b`
  - `a or b`
  - `not a`
  - `true`
  - `false`
  - both short-circuit
  - all return normalized integer booleans `0` or `1`
  - `true` and `false` are aliases for integer literals `1` and `0`
- Higher-order list builtins are available:
  - `list_map(xs, callback)`
  - `list_filter(xs, callback)`
- Core list mutation builtins also include:
  - `list_delete(xs, index)`
  - it mutates the list in place, shifts later items left, and returns the removed item
- Callbacks must currently have arity `1`.
- Top-level named functions can be used as function values in expression position.
- Function values can be stored in variables and passed around.
- Generic direct function-value calls are supported:
  - `f(10)`
- JIT, native executable, LLVM core Wasm, and LLVM `wasi:cli/command` component `main` may optionally take one argument:
  - `fn main(args) do ... end`
  - `args` is a list of strings containing the CLI arguments passed to the program
  - these runnable paths pass only actual CLI arguments, not the executable name
  - for CLI JIT runs, program arguments are passed after `--`
  - `main` currently supports at most one argument in these modes
- Method-call sugar is supported for exact-known receiver kinds:
  - `receiver.method(args...)`
  - it lowers at compile time to `{type_prefix}_{method}(receiver, args...)`
  - examples:
    - `"1234".is_integer()` -> `string_is_integer("1234")`
    - `m.keys()` -> `map_keys(m)`
    - `it.next()` -> `map_iter_next(it)`
  - user-defined helpers also work when they follow the same naming convention:
    - `fn string_wrap(s) do ... end`
    - `"x".wrap()`
  - unknown receiver kinds are compile errors
  - ambiguous receiver kinds are compile errors
  - `is_*` narrowing can make method calls resolvable inside guarded branches

## Build and Test

- Run tests:
  - `cargo test -q`
- Run one example with JIT:
  - `cargo run --release -q -- examples/<name>.expr --run-jit`
- Print inferred runtime kinds for an example:
  - `cargo run --release -q -- examples/<name>.expr --debug-types`
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
- String values use `TAG_STRING` and a dedicated heap object:
  - `{ len, cap, ptr }`
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
- Unix executable path uses `src/wrapper/unix.rs`.
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

