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
    env["CARGO_TARGET_DIR"] = str(REPO_ROOT / "target_llvm_backend")
    return env


def cranelift_env() -> dict[str, str]:
    env = os.environ.copy()
    env["CARGO_TARGET_DIR"] = str(REPO_ROOT / "target_matrix_cranelift")
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


def cargo_run_args(release: bool, extra: list[str]) -> list[str]:
    args = ["cargo", "run"]
    if release:
        args.append("--release")
    args.extend(["-q", "--"])
    args.extend(extra)
    return args


def cargo_run_args_llvm(release: bool, extra: list[str]) -> list[str]:
    args = ["cargo", "run"]
    if release:
        args.append("--release")
    args.extend(["-q", "--features", "llvm-backend", "--"])
    args.extend(extra)
    return args


def cargo_run_args_llvm_wasi(release: bool, extra: list[str]) -> list[str]:
    args = ["cargo", "run"]
    if release:
        args.append("--release")
    args.extend(["-q", "--features", "llvm-backend,wasi", "--"])
    args.extend(extra)
    return args


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
    if result.returncode != baseline_returncode:
        return (
            False,
            f"exit {result.returncode} != baseline {baseline_returncode}",
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


def run_cranelift_jit(example: Path, release: bool) -> subprocess.CompletedProcess[str]:
    return run_command(
        "cranelift-jit",
        cargo_run_args(release, [str(example), "--run-jit"]),
        env=cranelift_env(),
    )


def run_cranelift_native(
    example: Path, release: bool, staging_dir: Path
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = binary_path_for(staging_dir, example)
    compile_proc = run_command(
        "cranelift-native-compile",
        cargo_run_args(release, [str(example), "-o", str(output)]),
        env=cranelift_env(),
    )
    if compile_proc.returncode != 0:
        return compile_proc, 0
    run_proc = run_binary("cranelift-native-run", output)
    return run_proc, output.stat().st_size


def run_cranelift_run_ir(
    example: Path, release: bool
) -> subprocess.CompletedProcess[str]:
    return run_command(
        "cranelift-run-ir",
        cargo_run_args(release, [str(example), "--run-ir"]),
        env=cranelift_env(),
    )


def run_cranelift_emit_ir(
    example: Path, release: bool
) -> subprocess.CompletedProcess[str]:
    return run_command(
        "cranelift-emit-ir",
        cargo_run_args(release, [str(example), "--emit-ir"]),
        env=cranelift_env(),
    )


def run_llvm_jit(
    example: Path, release: bool, llvm_root: str | None
) -> subprocess.CompletedProcess[str]:
    return run_command(
        "llvm-jit",
        cargo_run_args_llvm(release, [str(example), "--run-jit", "--backend", "llvm"]),
        env=llvm_env(llvm_root),
    )


def run_llvm_native(
    example: Path, release: bool, llvm_root: str | None, staging_dir: Path
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = binary_path_for(staging_dir, example)
    compile_proc = run_command(
        "llvm-native-compile",
        cargo_run_args_llvm(
            release, [str(example), "--backend", "llvm", "-o", str(output)]
        ),
        env=llvm_env(llvm_root),
    )
    if compile_proc.returncode != 0:
        return compile_proc, 0
    run_proc = run_binary("llvm-native-run", output)
    return run_proc, output.stat().st_size


def run_llvm_wasm(
    example: Path, release: bool, llvm_root: str | None, staging_dir: Path
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = staging_dir / f"{example.stem}.wasm"
    compile_proc = run_command(
        "llvm-wasm-compile",
        cargo_run_args_llvm(
            release, [str(example), "--backend", "llvm", "-o", str(output)]
        ),
        env=llvm_env(llvm_root),
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
    example: Path, release: bool, llvm_root: str | None, staging_dir: Path
) -> tuple[subprocess.CompletedProcess[str], int]:
    output = staging_dir / f"{example.stem}.component.wasm"
    compile_proc = run_command(
        "llvm-component-compile",
        cargo_run_args_llvm_wasi(
            release, [str(example), "--backend", "llvm", "-o", str(output)]
        ),
        env=llvm_env(llvm_root),
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

    temp_dir = Path(tempfile.mkdtemp(prefix="expr-matrix-"))
    try:
        for example in examples:
            example_results: list[RunResult] = []
            baseline = run_cranelift_jit(example, args.release)
            baseline_stdout = normalize_output(baseline.stdout)
            baseline_name = f"{example.stem}:cranelift-jit"
            baseline_ok = baseline.returncode == 0
            baseline_result = RunResult(
                example=example.stem,
                name=baseline_name,
                ok=baseline_ok,
                returncode=baseline.returncode,
                stdout=baseline.stdout,
                stderr=baseline.stderr,
            )
            example_results.append(baseline_result)
            if not baseline_ok:
                failures.append(baseline_result)
                print_example_group(example.stem, example_results)
                continue

            for mode in args.modes:
                if mode == "cranelift-jit":
                    continue

                name = f"{example.stem}:{mode}"
                if mode == "cranelift-native":
                    proc, size = run_cranelift_native(example, args.release, temp_dir)
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
                    proc = run_cranelift_emit_ir(example, args.release)
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
                    proc = run_llvm_jit(example, args.release, args.llvm_root)
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
                    proc, size = run_llvm_native(
                        example, args.release, args.llvm_root, temp_dir
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
                    proc, size = run_llvm_wasm(
                        example, args.release, args.llvm_root, temp_dir
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
                    proc, size = run_llvm_component(
                        example, args.release, args.llvm_root, temp_dir
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
