#!/usr/bin/env python3

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


REPO_ROOT = Path(__file__).resolve().parent.parent
EXAMPLES_DIR = REPO_ROOT / "examples"
MAX_NATIVE_BINARY_SIZE = 50 * 1024


@dataclass
class RunResult:
    example: str
    name: str
    ok: bool
    returncode: int
    stdout: str
    stderr: str
    binary_size: int | None = None
    detail: str = ""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run a backend/mode matrix for expr examples."
    )
    parser.add_argument(
        "examples",
        nargs="*",
        help="Example names or paths. Defaults to all examples/*.expr",
    )
    parser.add_argument(
        "--modes",
        nargs="+",
        choices=[
            "cranelift-jit",
            "cranelift-native",
            "cranelift-emit-ir",
            "llvm-jit",
            "llvm-native",
            "llvm-wasm",
            "llvm-component",
        ],
        default=[
            "cranelift-jit",
            "cranelift-native",
            "cranelift-emit-ir",
            "llvm-jit",
            "llvm-native",
            "llvm-wasm",
            "llvm-component",
        ],
        help="Modes to execute",
    )
    parser.add_argument(
        "--llvm-root",
        help="LLVM build root used for llvm-backend runs",
    )
    parser.add_argument(
        "--release",
        action="store_true",
        help="Use release builds for cargo run commands",
    )
    parser.add_argument(
        "--keep-artifacts",
        action="store_true",
        help="Keep compiled binaries and IR files in the temp directory",
    )
    return parser.parse_args()


def resolve_examples(values: Iterable[str]) -> list[Path]:
    if not values:
        return sorted(EXAMPLES_DIR.glob("*.expr"))

    resolved: list[Path] = []
    for value in values:
        path = Path(value)
        if not path.suffix:
            path = EXAMPLES_DIR / f"{value}.expr"
        if not path.is_absolute():
            path = (REPO_ROOT / path).resolve() if not path.exists() else path.resolve()
        if not path.exists():
            raise SystemExit(f"example not found: {value}")
        resolved.append(path)
    return resolved


def llvm_env(llvm_root_arg: str | None) -> dict[str, str]:
    llvm_root_value = llvm_root_arg or os.environ.get("LLVM_SYS_201_PREFIX", "")
    if not llvm_root_value:
        raise SystemExit(
            "LLVM root not configured. Set LLVM_SYS_201_PREFIX or pass --llvm-root."
        )
    llvm_root = Path(llvm_root_value)
    llvm_bin = llvm_root / "bin"
    llvm_config = llvm_bin / "llvm-config"
    if os.name == "nt" and not llvm_config.exists():
        llvm_config = llvm_bin / "llvm-config.exe"
    if not llvm_config.exists():
        raise SystemExit(f"llvm-config not found at {llvm_config}")

    env = os.environ.copy()
    env["LLVM_SYS_201_PREFIX"] = str(llvm_root)
    env["LLVM_CONFIG_PATH"] = str(llvm_config)
    env["PATH"] = str(llvm_bin) + os.pathsep + env.get("PATH", "")
    env["CARGO_TARGET_DIR"] = os.environ.get(
        "MATRIX_LLVM_TARGET_DIR",
        str(REPO_ROOT / "target_llvm_backend"),
    )
    return env


def cranelift_env() -> dict[str, str]:
    env = os.environ.copy()
    env["CARGO_TARGET_DIR"] = os.environ.get(
        "MATRIX_CRANELIFT_TARGET_DIR",
        str(REPO_ROOT / "target_matrix_cranelift"),
    )
    return env


def run_command(
    name: str,
    argv: list[str],
    *,
    env: dict[str, str],
    cwd: Path = REPO_ROOT,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        argv,
        cwd=cwd,
        env=env,
        text=True,
        capture_output=True,
    )


def compiler_binary_path(target_dir: Path, release: bool) -> Path:
    profile = "release" if release else "debug"
    binary = "expr-compiler.exe" if os.name == "nt" else "expr-compiler"
    return target_dir / profile / binary


def cargo_build_args(release: bool, features: str | None = None) -> list[str]:
    args = ["cargo", "build"]
    if release:
        args.append("--release")
    if features:
        args.extend(["--features", features])
    return args


def ensure_compiler_built(
    *,
    release: bool,
    env: dict[str, str],
    features: str | None = None,
) -> Path:
    target_dir = Path(env["CARGO_TARGET_DIR"])
    binary = compiler_binary_path(target_dir, release)
    build_proc = run_command(
        "cargo-build",
        cargo_build_args(release, features),
        env=env,
    )
    if build_proc.returncode != 0:
        stderr = build_proc.stderr.rstrip()
        stdout = build_proc.stdout.rstrip()
        detail = stderr or stdout or "cargo build failed"
        raise SystemExit(detail)
    if not binary.exists():
        raise SystemExit(f"compiler binary not found after build: {binary}")
    return binary


def run_compiler(
    compiler: Path,
    extra: list[str],
    *,
    env: dict[str, str],
) -> subprocess.CompletedProcess[str]:
    return run_command(
        compiler.name,
        [str(compiler), *extra],
        env=env,
    )


def binary_path_for(staging_dir: Path, example: Path) -> Path:
    stem = example.stem
    if os.name == "nt":
        return staging_dir / f"{stem}.exe"
    return staging_dir / stem


def run_binary(name: str, path: Path) -> subprocess.CompletedProcess[str]:
    argv = [str(path)]
    return subprocess.run(argv, text=True, capture_output=True)


def js_runtime() -> str:
    configured = os.environ.get("JS_RUNTIME", "")
    if configured:
        return configured

    for candidate in ("node", "nodejs", "bun"):
        resolved = shutil.which(candidate)
        if resolved:
            return resolved

    raise SystemExit(
        "No JavaScript runtime found. Set JS_RUNTIME or ensure one of "
        "'node', 'nodejs', or 'bun' is in PATH."
    )


def wasmtime_runtime() -> str:
    configured = os.environ.get("WASMTIME", "")
    if configured:
        return configured

    resolved = shutil.which("wasmtime")
    if resolved:
        return resolved

    raise SystemExit("No Wasmtime runtime found. Set WASMTIME or ensure 'wasmtime' is in PATH.")


def normalize_output(text: str) -> str:
    return text.replace("\r\n", "\n")


def compare_to_baseline(
    result: subprocess.CompletedProcess[str],
    *,
    baseline_stdout: str,
    baseline_returncode: int,
    allow_different_stdout: bool = False,
) -> tuple[bool, str]:
    if baseline_returncode == 0:
        if result.returncode != 0:
            return (
                False,
                f"exit {result.returncode} != baseline 0",
            )
    else:
        if result.returncode == 0:
            return (
                False,
                f"exit 0 != expected non-zero baseline {baseline_returncode}",
            )
    if (
        not allow_different_stdout
        and normalize_output(result.stdout) != baseline_stdout
    ):
        return (False, "stdout differs from baseline")
    return (True, "")


def check_binary_size(size: int) -> tuple[bool, str]:
    if size > MAX_NATIVE_BINARY_SIZE:
        return (
            False,
            f"binary size {size} exceeds limit {MAX_NATIVE_BINARY_SIZE}",
        )
    return (True, "")


def run_cranelift_jit(
    compiler: Path, example: Path, env: dict[str, str]
) -> subprocess.CompletedProcess[str]:
    return run_compiler(
        compiler,
        [str(example), "--run-jit"],
        env=env,
    )


def run_cranelift_native(
    compiler: Path,
    example: Path,
    staging_dir: Path,
    env: dict[str, str],
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = binary_path_for(staging_dir, example)
    compile_proc = run_compiler(
        compiler,
        [str(example), "-o", str(output)],
        env=env,
    )
    if compile_proc.returncode != 0:
        return compile_proc, 0
    run_proc = run_binary("cranelift-native-run", output)
    return run_proc, output.stat().st_size


def run_cranelift_run_ir(
    compiler: Path, example: Path, env: dict[str, str]
) -> subprocess.CompletedProcess[str]:
    return run_compiler(
        compiler,
        [str(example), "--run-ir"],
        env=env,
    )


def run_cranelift_emit_ir(
    compiler: Path, example: Path, env: dict[str, str]
) -> subprocess.CompletedProcess[str]:
    return run_compiler(
        compiler,
        [str(example), "--emit-ir"],
        env=env,
    )


def run_llvm_jit(
    compiler: Path, example: Path, env: dict[str, str]
) -> subprocess.CompletedProcess[str]:
    return run_compiler(
        compiler,
        [str(example), "--run-jit", "--backend", "llvm"],
        env=env,
    )


def run_llvm_native(
    compiler: Path,
    example: Path,
    staging_dir: Path,
    env: dict[str, str],
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = binary_path_for(staging_dir, example)
    compile_proc = run_compiler(
        compiler,
        [str(example), "--backend", "llvm", "-o", str(output)],
        env=env,
    )
    if compile_proc.returncode != 0:
        return compile_proc, 0
    run_proc = run_binary("llvm-native-run", output)
    return run_proc, output.stat().st_size


def run_llvm_wasm(
    compiler: Path,
    example: Path,
    staging_dir: Path,
    env: dict[str, str],
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = staging_dir / f"{example.stem}.wasm"
    compile_proc = run_compiler(
        compiler,
        [str(example), "--backend", "llvm", "-o", str(output)],
        env=env,
    )
    if compile_proc.returncode != 0:
        return compile_proc, 0
    run_proc = run_command(
        "llvm-wasm-run",
        [js_runtime(), str(REPO_ROOT / "scripts" / "run-wasm.js"), str(output)],
        env=os.environ.copy(),
    )
    return run_proc, output.stat().st_size


def run_llvm_component(
    compiler: Path,
    example: Path,
    staging_dir: Path,
    env: dict[str, str],
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = staging_dir / f"{example.stem}.component.wasm"
    compile_proc = run_compiler(
        compiler,
        [str(example), "--backend", "llvm", "-o", str(output)],
        env=env,
    )
    if compile_proc.returncode != 0:
        return compile_proc, 0
    run_proc = run_command(
        "llvm-component-run",
        [wasmtime_runtime(), "run", str(output)],
        env=os.environ.copy(),
    )
    return run_proc, output.stat().st_size


def summarize(result: RunResult) -> str:
    status = "PASS" if result.ok else "FAIL"
    size = f" size={result.binary_size}" if result.binary_size is not None else ""
    detail = f" {result.detail}" if result.detail else ""
    return f"{status:4} {result.name:18} exit={result.returncode}{size}{detail}"


def print_example_group(example_name: str, group: list[RunResult]) -> None:
    print(f"{example_name}:")
    for result in group:
        mode_name = result.name.split(":", 1)[1]
        display_result = RunResult(
            example=result.example,
            name=mode_name,
            ok=result.ok,
            returncode=result.returncode,
            stdout=result.stdout,
            stderr=result.stderr,
            binary_size=result.binary_size,
            detail=result.detail,
        )
        print(f"  {summarize(display_result)}")
    print()


def main() -> int:
    args = parse_args()
    examples = resolve_examples(args.examples)
    failures: list[RunResult] = []
    cranelift_build_env = cranelift_env()
    llvm_build_env = llvm_env(args.llvm_root) if any(
        mode.startswith("llvm-") for mode in args.modes
    ) else None
    llvm_wasi_build_env = llvm_env(args.llvm_root) if "llvm-component" in args.modes else None

    cranelift_compiler = ensure_compiler_built(
        release=args.release,
        env=cranelift_build_env,
    )
    llvm_compiler = (
        ensure_compiler_built(
            release=args.release,
            env=llvm_build_env,
            features="llvm-backend",
        )
        if llvm_build_env is not None
        else None
    )
    llvm_wasi_compiler = (
        ensure_compiler_built(
            release=args.release,
            env=llvm_wasi_build_env,
            features="llvm-backend,wasi",
        )
        if llvm_wasi_build_env is not None
        else None
    )

    temp_dir = Path(tempfile.mkdtemp(prefix="expr-matrix-"))
    try:
        for example in examples:
            example_results: list[RunResult] = []
            baseline = run_cranelift_jit(
                cranelift_compiler, example, cranelift_build_env
            )
            baseline_stdout = normalize_output(baseline.stdout)
            baseline_name = f"{example.stem}:cranelift-jit"
            baseline_result = RunResult(
                example=example.stem,
                name=baseline_name,
                ok=True,
                returncode=baseline.returncode,
                stdout=baseline.stdout,
                stderr=baseline.stderr,
            )
            example_results.append(baseline_result)

            for mode in args.modes:
                if mode == "cranelift-jit":
                    continue

                name = f"{example.stem}:{mode}"
                if mode == "cranelift-native":
                    proc, size = run_cranelift_native(
                        cranelift_compiler, example, temp_dir, cranelift_build_env
                    )
                    ok, detail = compare_to_baseline(
                        proc,
                        baseline_stdout=baseline_stdout,
                        baseline_returncode=baseline.returncode,
                    )
                    if ok and proc.returncode == 0:
                        ok, detail = check_binary_size(size)
                    result = RunResult(
                        example=example.stem,
                        name=name,
                        ok=ok,
                        returncode=proc.returncode,
                        stdout=proc.stdout,
                        stderr=proc.stderr,
                        binary_size=size if proc.returncode == 0 else None,
                        detail=detail,
                    )
                elif mode == "cranelift-emit-ir":
                    proc = run_cranelift_emit_ir(
                        cranelift_compiler, example, cranelift_build_env
                    )
                    ir_ok = proc.returncode == 0 and "function" in proc.stdout
                    detail = "" if ir_ok else "emit-ir output missing function"
                    result = RunResult(
                        example=example.stem,
                        name=name,
                        ok=ir_ok,
                        returncode=proc.returncode,
                        stdout=proc.stdout,
                        stderr=proc.stderr,
                        detail=detail,
                    )
                elif mode == "llvm-jit":
                    assert llvm_compiler is not None and llvm_build_env is not None
                    proc = run_llvm_jit(llvm_compiler, example, llvm_build_env)
                    ok, detail = compare_to_baseline(
                        proc,
                        baseline_stdout=baseline_stdout,
                        baseline_returncode=baseline.returncode,
                    )
                    result = RunResult(
                        example=example.stem,
                        name=name,
                        ok=ok,
                        returncode=proc.returncode,
                        stdout=proc.stdout,
                        stderr=proc.stderr,
                        detail=detail,
                    )
                elif mode == "llvm-native":
                    assert llvm_compiler is not None and llvm_build_env is not None
                    proc, size = run_llvm_native(
                        llvm_compiler, example, temp_dir, llvm_build_env
                    )
                    ok, detail = compare_to_baseline(
                        proc,
                        baseline_stdout=baseline_stdout,
                        baseline_returncode=baseline.returncode,
                    )
                    if ok and proc.returncode == 0:
                        ok, detail = check_binary_size(size)
                    result = RunResult(
                        example=example.stem,
                        name=name,
                        ok=ok,
                        returncode=proc.returncode,
                        stdout=proc.stdout,
                        stderr=proc.stderr,
                        binary_size=size if proc.returncode == 0 else None,
                        detail=detail,
                    )
                elif mode == "llvm-wasm":
                    assert llvm_compiler is not None and llvm_build_env is not None
                    proc, size = run_llvm_wasm(
                        llvm_compiler, example, temp_dir, llvm_build_env
                    )
                    ok, detail = compare_to_baseline(
                        proc,
                        baseline_stdout=baseline_stdout,
                        baseline_returncode=baseline.returncode,
                    )
                    result = RunResult(
                        example=example.stem,
                        name=name,
                        ok=ok,
                        returncode=proc.returncode,
                        stdout=proc.stdout,
                        stderr=proc.stderr,
                        binary_size=size if proc.returncode == 0 else None,
                        detail=detail,
                    )
                elif mode == "llvm-component":
                    assert (
                        llvm_wasi_compiler is not None and llvm_wasi_build_env is not None
                    )
                    proc, size = run_llvm_component(
                        llvm_wasi_compiler, example, temp_dir, llvm_wasi_build_env
                    )
                    ok, detail = compare_to_baseline(
                        proc,
                        baseline_stdout=baseline_stdout,
                        baseline_returncode=baseline.returncode,
                    )
                    result = RunResult(
                        example=example.stem,
                        name=name,
                        ok=ok,
                        returncode=proc.returncode,
                        stdout=proc.stdout,
                        stderr=proc.stderr,
                        binary_size=size if proc.returncode == 0 else None,
                        detail=detail,
                    )
                else:
                    raise AssertionError(f"unsupported mode: {mode}")

                example_results.append(result)
                if not result.ok:
                    failures.append(result)

            print_example_group(example.stem, example_results)

        if failures:
            print("\nFailures:", file=sys.stderr)
            for result in failures:
                print(f"- {result.name}", file=sys.stderr)
                if result.detail:
                    print(f"  detail: {result.detail}", file=sys.stderr)
                if result.stderr.strip():
                    print("  stderr:", file=sys.stderr)
                    print(result.stderr.rstrip(), file=sys.stderr)
                if result.stdout.strip():
                    print("  stdout:", file=sys.stderr)
                    print(result.stdout.rstrip(), file=sys.stderr)
            if args.keep_artifacts:
                print(f"\nArtifacts kept in {temp_dir}", file=sys.stderr)
            return 1

        if args.keep_artifacts:
            keep_target = REPO_ROOT / ".matrix-artifacts"
            if keep_target.exists():
                shutil.rmtree(keep_target)
            shutil.copytree(temp_dir, keep_target)
            print(f"\nArtifacts copied to {keep_target}")

        return 0
    finally:
        if not args.keep_artifacts:
            shutil.rmtree(temp_dir, ignore_errors=True)


if __name__ == "__main__":
    raise SystemExit(main())
