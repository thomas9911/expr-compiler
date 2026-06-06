# another programming language (or expression thing)

Concept, have compiled output plus JIT and IR execution paths.

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
- `bigint_bitand(a, b)`, `bigint_bitor(a, b)`, `bigint_bitxor(a, b)`,
  `bigint_shl(a, shift)`, and `bigint_shr(a, shift)` are also available
- comparison operators also work for `BigInt` values when both operands are bigint:
  - `==`, `!=`, `<`, `<=`, `>`, `>=`
- `+`, `-`, `*`, `/`, and `%` also work for `BigInt` values when both operands are bigint
- `&`, `|`, `^`, `<<`, and `>>` also work for supported bigint bitwise forms
- mixed `Int` / `BigInt` arithmetic and comparisons now promote the `Int` operand for operator use
- mixed `Int` / `BigInt` bitwise `&`, `|`, and `^` also promote the `Int` operand
- bigint `<<` and `>>` currently use a bigint-or-int left operand and an `Int` shift count
- bigint bitwise support currently targets non-negative values first; negative bigint operands trap at runtime
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
    - `string_from_codepoints(xs)`
    - `type_of(value)`
    - `string_repeat(s, n)`
    - `string_reverse(s)`
  - runtime type predicates are also available as builtins:
    - `is_int(value)`
    - `is_bigint(value)`
    - `is_string(value)`
    - `is_list(value)`
    - `is_map(value)`
    - `is_function(value)`
    - `is_string_iter(value)`
  - `type_of(value)` returns a debuggable stable type name such as `"int"`, `"bigint"`, `"string"`, `"list"`, `"map"`, `"function"`, or `"string_iter"`
  - `string_try_parse_integer(s)` returns `(ok, value, err)` where `ok` is `true`/`false`, `value` is the parsed `Int` or `0`, and `err` is `""` on success or a short error message on failure
  - `string_try_parse_bigint(s)` returns `(ok, value, err)` where `value` is the parsed `BigInt` or `bigint_from_int(0)`
  - `string_try_first(s)` returns `(ok, value, err)` where `value` is the first byte as an `Int`
  - `string_try_last(s)` returns `(ok, value, err)` where `value` is the last byte as an `Int`
  - `bytes_try_get(s, index)` returns `(ok, value, err)` where `value` is the byte at `index` as an `Int`
  - `string_try_pop(s)` returns `(ok, value, err)` and mutates `s` on success
  - `string_from_codepoints(xs)` takes a `list<int>` of Unicode codepoints and returns a UTF-8 string
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
- the compiler now also performs conservative compile-time kind checks for obvious builtin misuse and indexing misuse
  - examples:
    - `bytes_len(1)`
    - `list_len("abc")`
    - `1[0]`
    - `xs["0"]`
  - it only rejects when the inferred kinds are confident; unknown values still use runtime checks
- UTF-8-aware indexing/slicing is not implemented yet

## Maps

The language has a mutable `Map` runtime type with string keys.

Current map surface:

```text
m = map_new()
map_set(m, "name", "expr-compiler")
print(map_len(m))
print(map_has(m, "name"))
print(map_get(m, "name"))
ok, value, err = map_try_get(m, "missing")
ok, key, value = map_try_pop(m)
```

Current behavior:

- keys are strings
- values can be any runtime value
- `map_set(m, key, value)` mutates the map in place
- `map_get(m, key)` traps when the key is missing
- `map_delete(m, key)` removes the key and returns the removed value
- `map_keys(m)` returns a list of string keys
- `map_try_get(m, key)` returns `(ok, value, err)` with `err == "missing key"` on failure
- `map_try_delete(m, key)` returns `(ok, value, err)` with the same missing-key contract
- `map_try_pop(m)` returns `(ok, key, value)` and removes an arbitrary entry on success
- `map_update(m, key, callback)` updates an existing entry with `callback(value)` and returns `true`/`false`
- `map_update_or_default(m, key, default_value, callback)` stores and returns `callback(current_or_default_value)`
- empty `map_try_pop(m)` returns `(false, "", 0)`
- map literals are supported:
  - `{}`
  - `{ name: "x", count: 1 }`
  - `{ dynamic_key => 1 }`

Logical infix operators are also supported:

- `a and b`
- `a or b`
- `not a`
- `true`
- `false`
- both short-circuit
- all return normalized integer booleans `0` or `1`
- `true` and `false` are parser aliases for integer literals `1` and `0`

## Executable arguments

JIT, native executable, core Wasm, and `wasi:cli/command` component `main` may optionally take one argument:

```text
fn main(args) do
    print(list_len(args))
    print(list_get(args, 0))
    0
end
```

Current behavior:

- this applies to JIT execution, native executable output, LLVM core Wasm output, and LLVM `wasi:cli/command` component output
- `args` is a list of strings
- only actual CLI arguments are passed; the executable name is omitted
- `main` currently supports at most one argument in these runnable output modes
- for JIT execution through the CLI, pass program arguments after `--`, for example:
  - `cargo run --release -q -- examples/args.expr --run-jit -- hello world`

## Debugging inferred kinds

You can print the original source annotated with inferred runtime value kinds:

```text
cargo run --release -q -- examples/strings.expr --debug-types
```

Current behavior:

- prints the original source
- adds `#?` annotation lines
- shows function return kinds
- shows function input kinds when present
- shows assignment and destructuring variable kinds
- this is based on conservative value-kind inference, not a full static type system

## Higher-order list functions

The language supports anonymous functions, captured closures, and higher-order
list operations.

Supported forms:

```text
list_map(xs, fn item -> item * 2 end)
list_filter(xs, fn item -> item > 2 end)
list_filter(xs, fn item -> item == limit end)
list_delete(xs, 1)
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

- `list_delete(xs, index)` mutates the list in place, shifts later items left, and returns the removed item
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
