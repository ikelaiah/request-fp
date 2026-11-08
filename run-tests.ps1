<#
.SYNOPSIS
  Build and run the Request-FP test suite against the local HTTP fixture.

.PARAMETER LeakCheck
  Build the Debug mode (heaptrc, range/overflow checks) and fail if any
  memory is left unfreed.

.PARAMETER Fpc
  Build with fpc directly (used automatically when lazbuild is missing).

.PARAMETER Port
  Fixture port (default 18080).

.PARAMETER RunnerArgs
  Arguments for TestRunner (default: -a --format=plain).

.NOTES
  Set REQUEST_FP_TEST_BASE_URL to test against another server (for example
  https://httpbin.org); the local fixture is then not started.
#>
[CmdletBinding()]
param(
  [switch]$LeakCheck,
  [switch]$Fpc,
  [int]$Port = 18080,
  [string[]]$RunnerArgs = @('-a', '--format=plain')
)

$ErrorActionPreference = 'Stop'
$repoRoot = $PSScriptRoot
$buildMode = if ($LeakCheck) { 'Debug' } else { 'Release' }

$useFpc = $Fpc -or -not (Get-Command lazbuild -ErrorAction SilentlyContinue)
if ($useFpc -and -not (Get-Command fpc -ErrorAction SilentlyContinue)) {
  throw 'Neither lazbuild nor fpc was found. Install FPC 3.2.2+ or Lazarus 4.8+.'
}

$python = $null
foreach ($candidate in @('python', 'python3', 'py')) {
  if (Get-Command $candidate -ErrorAction SilentlyContinue) {
    & $candidate -c 'import sys' 2>$null | Out-Null
    if ($LASTEXITCODE -eq 0) {
      $python = $candidate
      break
    }
  }
}
if (-not $python) {
  throw 'Python 3 was not found; it is needed for tests\http_fixture.py.'
}

$runner = Join-Path $repoRoot 'tests\TestRunner.exe'

if ($useFpc) {
  $target = (& fpc -iTP).Trim() + '-' + (& fpc -iTO).Trim()
  $unitDir = Join-Path $repoRoot "tests\lib\fpc-$target-$buildMode"
  New-Item -ItemType Directory -Force -Path $unitDir | Out-Null
  # Mirrors the Release and Debug modes in tests\TestRunner.lpi.
  $fpcArgs = @('-B', '-MObjFPC', '-Scghi', '-FcUTF8', '-v0', '-l-',
    ('-Fu' + (Join-Path $repoRoot 'src')),
    ('-Fu' + (Join-Path $repoRoot 'tests')),
    ('-FU' + $unitDir),
    ('-o' + $runner))
  if ($LeakCheck) {
    $fpcArgs += @('-gh', '-gl', '-gw3', '-gt', '-Ci', '-Cr', '-Co', '-Ct', '-Sa')
  }
  else {
    $fpcArgs += '-O2'
  }
  Write-Output "Building tests\TestRunner.lpr with fpc ($buildMode)"
  & fpc @fpcArgs (Join-Path $repoRoot 'tests\TestRunner.lpr')
}
else {
  Write-Output "Building tests\TestRunner.lpi ($buildMode)"
  & lazbuild '--quiet' '--quiet' '--build-all' "--build-mode=$buildMode" `
    '--no-write-project' (Join-Path $repoRoot 'tests\TestRunner.lpi')
}
if ($LASTEXITCODE -ne 0) {
  throw "Test runner build failed with exit code $LASTEXITCODE."
}
$fixture = $null
$heapLog = Join-Path ([System.IO.Path]::GetTempPath()) (
  'request-fp-heaptrc-' + [System.Guid]::NewGuid().ToString('N') + '.log')
$savedBaseUrl = $env:REQUEST_FP_TEST_BASE_URL
$savedHeaptrc = $env:HEAPTRC
$status = 1

try {
  if (-not $env:REQUEST_FP_TEST_BASE_URL) {
    $baseUrl = "http://127.0.0.1:$Port"
    $fixture = Start-Process -FilePath $python -PassThru -WindowStyle Hidden `
      -ArgumentList @((Join-Path $repoRoot 'tests\http_fixture.py'), '--port', "$Port")

    $ready = $false
    for ($attempt = 0; $attempt -lt 100; $attempt++) {
      try {
        $response = Invoke-WebRequest -Uri "$baseUrl/health" -UseBasicParsing -TimeoutSec 1
        if ($response.StatusCode -eq 200) {
          $ready = $true
          break
        }
      }
      catch {
        if ($fixture.HasExited) {
          break
        }
        Start-Sleep -Milliseconds 100
      }
    }
    if (-not $ready) {
      throw "Local HTTP fixture failed to start on port $Port."
    }
    $env:REQUEST_FP_TEST_BASE_URL = $baseUrl
  }
  Write-Output "Testing against $env:REQUEST_FP_TEST_BASE_URL"

  if ($LeakCheck) {
    $env:HEAPTRC = "log=$heapLog"
  }

  & $runner @RunnerArgs
  $status = $LASTEXITCODE

  if ($LeakCheck) {
    if (-not (Test-Path -LiteralPath $heapLog)) {
      throw 'Leak check failed: no heaptrc report was written.'
    }
    $report = Get-Content -LiteralPath $heapLog -Raw
    $match = [regex]::Match($report, '(?m)^(\d+) unfreed memory blocks')
    if (-not $match.Success) {
      Write-Output $report
      throw 'Leak check failed: could not read the heaptrc report.'
    }
    $unfreed = [int]$match.Groups[1].Value
    if ($unfreed -ne 0) {
      Write-Output $report
      throw "Leak check failed: $unfreed unfreed memory blocks."
    }
    Write-Output 'Leak check passed: 0 unfreed memory blocks.'
  }
}
finally {
  $env:REQUEST_FP_TEST_BASE_URL = $savedBaseUrl
  $env:HEAPTRC = $savedHeaptrc
  if ($fixture -and -not $fixture.HasExited) {
    Stop-Process -Id $fixture.Id -Force -ErrorAction SilentlyContinue
  }
  Remove-Item -LiteralPath $heapLog -Force -ErrorAction SilentlyContinue
}

exit $status
