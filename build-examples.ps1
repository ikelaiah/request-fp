[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'

$RepoRoot = [System.IO.Path]::GetFullPath($PSScriptRoot)
$ExamplesDir = Join-Path $RepoRoot 'examples'
$OutputDir = Join-Path $RepoRoot 'example-bin'
$ExpectedOutputDir = [System.IO.Path]::GetFullPath(
  (Join-Path $RepoRoot 'example-bin')
)

if ([System.IO.Path]::GetFullPath($OutputDir) -ne $ExpectedOutputDir) {
  throw "Refusing to clean unexpected output directory: $OutputDir"
}

$LazBuild = Get-Command lazbuild -ErrorAction SilentlyContinue
if ($null -eq $LazBuild) {
  throw 'lazbuild was not found. Install Lazarus 4.8+ and add lazbuild to PATH.'
}

$Projects = @(
  Get-ChildItem -LiteralPath $ExamplesDir -Recurse -File -Filter '*.lpi' |
    Where-Object {
      $_.FullName -notmatch '[\\/]backup[\\/]'
    } |
    Sort-Object FullName
)

if ($Projects.Count -eq 0) {
  throw "No Lazarus example projects were found under $ExamplesDir"
}

if (Test-Path -LiteralPath $OutputDir) {
  Remove-Item -LiteralPath $OutputDir -Recurse -Force
}
New-Item -ItemType Directory -Path $OutputDir | Out-Null

foreach ($Project in $Projects) {
  $RelativeProject = $Project.FullName.Substring($RepoRoot.Length)
  $RelativeProject = $RelativeProject.TrimStart([char[]]'\/')
  Write-Host "Building $RelativeProject"

  & $LazBuild.Source `
    '--quiet' `
    '--quiet' `
    '--build-all' `
    '--build-mode=Release' `
    '--no-write-project' `
    "--opt=-FE$OutputDir" `
    $Project.FullName

  if ($LASTEXITCODE -ne 0) {
    throw "Failed to build $RelativeProject"
  }
}

$BuiltFiles = @(
  Get-ChildItem -LiteralPath $OutputDir -File |
    Sort-Object Name
)

Write-Host ''
Write-Host "Built $($Projects.Count) example projects into $OutputDir"
foreach ($BuiltFile in $BuiltFiles) {
  Write-Host "  $($BuiltFile.Name)"
}
