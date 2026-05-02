param(
    [ValidateSet("test", "build", "run", "example", "compile")]
    [string]$Action = "test",
    [string]$Input,
    [string]$LlvmRoot,
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$ExtraArgs
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$defaultLlvmRoot = "F:\Rust\llvm-project-20.1.8.src\llvm-project-20.1.8.src\build"
$llvmBuildRoot = if (-not [string]::IsNullOrWhiteSpace($LlvmRoot)) {
    $LlvmRoot
} elseif (-not [string]::IsNullOrWhiteSpace($env:LLVM_SYS_201_PREFIX)) {
    $env:LLVM_SYS_201_PREFIX
} else {
    $defaultLlvmRoot
}
$llvmBin = Join-Path $llvmBuildRoot "bin"
$llvmLib = Join-Path $llvmBuildRoot "lib"
$llvmConfig = Join-Path $llvmBin "llvm-config.exe"
$env:CARGO_TARGET_DIR = Join-Path $repoRoot "target_llvm_backend"

if (-not (Test-Path -LiteralPath $llvmConfig)) {
    throw "llvm-config.exe not found at $llvmConfig"
}

$rustHost = (& rustc -vV | Select-String "^host: ").ToString().Split(":", 2)[1].Trim()
$llvmHost = (& $llvmConfig --host-target).Trim()
if ($llvmHost -ne $rustHost) {
    throw "LLVM host target mismatch: rustc targets '$rustHost' but llvm-config reports '$llvmHost'. Rebuild LLVM for $rustHost or use a matching Rust toolchain."
}

$env:LLVM_SYS_201_PREFIX = $llvmBuildRoot
$env:LLVM_CONFIG_PATH = $llvmConfig
if (-not ($env:PATH -split ";" | Where-Object { $_ -eq $llvmBin })) {
    $env:PATH = "$llvmBin;$env:PATH"
}

Write-Host "Using LLVM from $llvmBuildRoot"

$cargoArgs = @()
switch ($Action) {
    "test" {
        $cargoArgs = @("test", "-q", "--features", "llvm-backend")
    }
    "build" {
        $cargoArgs = @("build", "--features", "llvm-backend")
    }
    "run" {
        if ([string]::IsNullOrWhiteSpace($Input)) {
            throw "Action 'run' requires -Input <path-to-.expr>"
        }
        $cargoArgs = @(
            "run", "--release", "-q", "--features", "llvm-backend", "--",
            $Input, "--run-jit", "--backend", "llvm"
        )
    }
    "example" {
        if ([string]::IsNullOrWhiteSpace($Input)) {
            throw "Action 'example' requires -Input <example-name-or-path>"
        }
        $examplePath = if ($Input.EndsWith(".expr")) {
            $Input
        } else {
            Join-Path $repoRoot "examples\$Input.expr"
        }
        $cargoArgs = @(
            "run", "--release", "-q", "--features", "llvm-backend", "--",
            $examplePath, "--run-jit", "--backend", "llvm"
        )
    }
    "compile" {
        if ([string]::IsNullOrWhiteSpace($Input)) {
            throw "Action 'compile' requires -Input <path-to-.expr>"
        }
        $cargoArgs = @(
            "run", "--release", "-q", "--features", "llvm-backend", "--",
            $Input, "--backend", "llvm"
        )
    }
}

if ($ExtraArgs.Count -gt 0) {
    $cargoArgs += $ExtraArgs
}

Push-Location $repoRoot
try {
    & cargo @cargoArgs
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}
finally {
    Pop-Location
}
