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
- `BigInt`
- `String`
- `Function`

## BigInt

The language has a `BigInt` runtime type backed by a dedicated heap object with
`u32` limbs.

Current construction and arithmetic surface:

```text
a = 2147483647n
b = 2147483647n
c = bigint_from_int(100)
d = bigint_from_int(7)

print(bigint_compare(a, b))
print(a < b)
print(a + b)
print(a * bigint_from_int(2))
print(c / d)
print(bigint_subtract(b, a))
```

Current behavior:

- `bigint_from_int(x)` constructs a bigint from an `Int`
- `123n` is a bigint literal
- `bigint_compare(a, b)` returns `-1`, `0`, or `1`
- `bigint_add(a, b)`, `bigint_subtract(a, b)`, `bigint_multiply(a, b)`,
  `bigint_divide(a, b)`, and `bigint_modulo(a, b)` are available as builtins
- comparison operators also work for `BigInt` values when both operands are bigint:
  - `==`, `!=`, `<`, `<=`, `>`, `>=`
- `+`, `-`, `*`, `/`, and `%` also work for `BigInt` values when both operands are bigint
- mixed `Int` / `BigInt` arithmetic and comparisons now promote the `Int` operand for operator use
- plain `Int` / `Int` arithmetic keeps the existing semantics, including overflow traps
- the explicit `bigint_*` builtins now accept `Int`, `BigInt`, or mixed `Int` / `BigInt` operands by promoting `Int` arguments to `BigInt`
- bigint arithmetic is implemented in Cranelift IR and LLVM IR, not in Rust runtime helpers

## Strings

The language has a `String` runtime type backed by a dedicated heap object with
`len`, `cap`, and `ptr` fields. String values are still UTF-8 byte storage, and
the current API surface is explicitly byte-oriented.

Current string surface:

```text
print("hello")
print("line1\nline2")
print(string_concat("ab", "cd"))
print("abc" == "abc")
print("abc" != "xyz")
print(bytes_len("hello"))
print(bytes_get("hello", 1))
print(bytes_slice("hello", 1, 4))
bytes_push(s, 33)
bytes_set(s, 0, 72)
copy = string_copy(s)
```

Current behavior:

- string literals are supported with basic escapes:
  - `\"`, `\\`, `\n`, `\r`, `\t`
- some higher-level helpers can be implemented in the language and autoloaded by the compiler on use
  - current examples:
    - `string_is_empty(s)`
    - `string_is_not_empty(s)`
    - `string_len(s)`
    - `string_first(s)`
    - `string_last(s)`
    - `string_starts_with(s, prefix)`
    - `string_ends_with(s, suffix)`
    - `string_contains(s, needle)`
    - `string_is_ascii(s)`
    - `string_all(s, predicate)`
    - `string_is_integer(s)`
    - `string_repeat(s, n)`
    - `string_reverse(s)`
- `print` can print string values
- `string_concat(a, b)` concatenates two strings and returns a fresh string
- `bytes_len(s)` returns the byte length as an `Int`
- `bytes_get(s, i)` returns the byte at index `i` as an `Int`
- `bytes_slice(s, start, end)` returns a new string over the byte range `[start, end)`
- `bytes_pop(s)` removes and returns the last byte as an `Int`
- `bytes_insert(s, index, byte)` inserts one byte in place, shifting later bytes right
- `bytes_remove(s, index)` removes and returns one byte as an `Int`, shifting later bytes left
- `bytes_push(s, byte)` appends one byte, growing capacity if needed
- `bytes_set(s, index, byte)` overwrites one byte in place
- `string_chars(s)` returns a UTF-8 code point iterator
- `string_iter_done(it)` returns truthy when the iterator is exhausted
- `string_iter_next(it)` returns the next Unicode scalar value as an `Int`
- `string_copy(s)` returns a fresh exact-fit copy of the visible string contents
- `==` and `!=` compare string byte contents
- `String == non-String` is false
- `String != non-String` is true
- `string_*` iteration validates UTF-8 and traps on invalid byte sequences
- UTF-8-aware indexing/slicing and string conversion helpers are not implemented yet

Logical infix operators are also supported:

- `a and b`
- `a or b`
- `not a`
- `true`
- `false`
- both short-circuit
- all return normalized integer booleans `0` or `1`
- `true` and `false` are parser aliases for integer literals `1` and `0`

## Higher-order list functions

The language supports anonymous functions, captured closures, and higher-order
list operations.

Supported forms:

```text
list_map(xs, fn item -> item * 2 end)
list_filter(xs, fn item -> item > 2 end)
list_filter(xs, fn item -> item == limit end)
```

Named top-level functions can also be used as function values:

```text
fn double(item) do
    item * 2
end

fn main() do
    xs = [1, 2, 3]
    ys = list_map(xs, double)
    print(ys)
end
```

Function values can be stored in variables and called directly:

```text
fn main() do
    factor = 3
    f = fn x -> x * factor end
    f(10)
end
```

Current constraints:

- `list_map` and `list_filter` currently require unary callbacks
- function values can be stored in variables and passed to `list_map` /
  `list_filter`
- direct function-value calls currently use identifier callees such as `f(10)`
- mixed `Int` / `BigInt` operator arithmetic and comparisons promote the `Int` operand

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
- expected-failure examples are supported too:
  - Cranelift JIT defines the baseline stdout and success/failure class
  - non-zero failure codes do not have to match exactly across native, Wasm, and component runners

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

Arena exhaustion now reports explicitly across the main runnable backends as:

```text
runtime error: out of arena memory
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
