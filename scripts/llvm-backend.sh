#!/usr/bin/env bash
set -euo pipefail

action="${1:-test}"
shift || true

input=""
llvm_root="${LLVM_SYS_201_PREFIX:-}"
extra_args=()

while (($# > 0)); do
    case "$1" in
        -i|--input)
            if (($# < 2)); then
                echo "missing value for $1" >&2
                exit 1
            fi
            input="$2"
            shift 2
            ;;
        --llvm-root)
            if (($# < 2)); then
                echo "missing value for $1" >&2
                exit 1
            fi
            llvm_root="$2"
            shift 2
            ;;
        --)
            shift
            extra_args+=("$@")
            break
            ;;
        *)
            extra_args+=("$1")
            shift
            ;;
    esac
done

case "$action" in
    test|build|run|example|compile)
        ;;
    *)
        echo "unknown action: $action" >&2
        exit 1
        ;;
esac

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/.." && pwd)"

if [[ -z "$llvm_root" ]]; then
    echo "LLVM root not configured. Set LLVM_SYS_201_PREFIX or pass --llvm-root." >&2
    exit 1
fi

llvm_bin="$llvm_root/bin"
llvm_config="$llvm_bin/llvm-config"
if [[ ! -x "$llvm_config" && -x "${llvm_config}.exe" ]]; then
    llvm_config="${llvm_config}.exe"
fi

if [[ ! -x "$llvm_config" ]]; then
    echo "llvm-config not found at $llvm_config" >&2
    exit 1
fi

rust_host="$(rustc -vV | awk -F': ' '/^host: / { print $2 }')"
llvm_host="$("$llvm_config" --host-target | tr -d '\r')"
if [[ "$llvm_host" != "$rust_host" ]]; then
    echo "LLVM host target mismatch: rustc targets '$rust_host' but llvm-config reports '$llvm_host'." >&2
    echo "Rebuild LLVM for $rust_host or use a matching Rust toolchain." >&2
    exit 1
fi

export LLVM_SYS_201_PREFIX="$llvm_root"
export LLVM_CONFIG_PATH="$llvm_config"
export PATH="$llvm_bin:$PATH"
export CARGO_TARGET_DIR="$repo_root/target_llvm_backend"

echo "Using LLVM from $llvm_root"

cargo_args=()
case "$action" in
    test)
        cargo_args=(test -q --features llvm-backend)
        ;;
    build)
        cargo_args=(build --features llvm-backend)
        ;;
    run)
        if [[ -z "$input" ]]; then
            echo "action 'run' requires --input <path-to-.expr>" >&2
            exit 1
        fi
        cargo_args=(run --release -q --features llvm-backend -- "$input" --run-jit --backend llvm)
        ;;
    example)
        if [[ -z "$input" ]]; then
            echo "action 'example' requires --input <example-name-or-path>" >&2
            exit 1
        fi
        example_path="$input"
        if [[ "$example_path" != *.expr ]]; then
            example_path="$repo_root/examples/$example_path.expr"
        fi
        cargo_args=(run --release -q --features llvm-backend -- run "$example_path" --backend llvm)
        ;;
    compile)
        if [[ -z "$input" ]]; then
            echo "action 'compile' requires --input <path-to-.expr>" >&2
            exit 1
        fi
        cargo_args=(run --release -q --features llvm-backend -- build "$input" --backend llvm)
        ;;
esac

if ((${#extra_args[@]} > 0)); then
    cargo_args+=("${extra_args[@]}")
fi

cd "$repo_root"
exec cargo "${cargo_args[@]}"
