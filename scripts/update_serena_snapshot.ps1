# Requires: PowerShell 5+
Param()

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-DirIfMissing {
    param([string]$Path)
    if (-not (Test-Path -LiteralPath $Path)) {
        New-Item -ItemType Directory -Path $Path | Out-Null
    }
}

function Write-Utf8 {
    param(
        [string]$Path,
        [string]$Content
    )
    $dir = Split-Path -Parent $Path
    New-DirIfMissing -Path $dir
    # Use UTF8 without BOM for portability (PowerShell 5.1 compatible)
    $utf8NoBom = New-Object -TypeName System.Text.UTF8Encoding -ArgumentList $false
    [System.IO.File]::WriteAllText($Path, $Content, $utf8NoBom)
}

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot '..')
$repoRoot = $repoRoot.Path
$memDir = Join-Path $repoRoot '.serena/memories'
New-DirIfMissing $memDir

$date = Get-Date -Format 'yyyy-MM-dd'

# Helper to relativize paths
function To-Rel {
    param([string]$Full)
    if ($Full.StartsWith($repoRoot)) {
        return $Full.Substring($repoRoot.Length + 1).Replace('\\','/')
    }
    return $Full.Replace('\\','/')
}

# Collect top-level directories and files
$topDirs = Get-ChildItem -LiteralPath $repoRoot -Directory | ForEach-Object { $_.Name } | Sort-Object
$topFiles = Get-ChildItem -LiteralPath $repoRoot -File | ForEach-Object { $_.Name } | Sort-Object

# Key files
$keyFilesList = @(
    'CMakeLists.txt','CMakePresets.json','CMakeUserPresets.json',
    'conanfile.txt','README.md','compile_commands.json'
) | ForEach-Object {
    $p = Join-Path $repoRoot $_
    if (Test-Path -LiteralPath $p) { $_ }
}

# Source files
$srcFiles = @()
if (Test-Path (Join-Path $repoRoot 'src')) {
    $srcFiles = Get-ChildItem -LiteralPath (Join-Path $repoRoot 'src') -Recurse -File -Include *.cpp,*.cc,*.cxx,*.c,*.hpp,*.hh,*.h | ForEach-Object { To-Rel $_.FullName }
}

# Grammar files
$grammarFiles = @()
if (Test-Path (Join-Path $repoRoot 'grammar')) {
    $grammarFiles = Get-ChildItem -LiteralPath (Join-Path $repoRoot 'grammar') -Recurse -File -Include *.g4 | ForEach-Object { To-Rel $_.FullName }
}

# Executables (common locations)
$executables = @()
foreach ($dir in @('build','build-ninja')) {
    $path = Join-Path $repoRoot $dir
    if (Test-Path $path) {
        $executables += Get-ChildItem -LiteralPath $path -Recurse -File -Include *.exe -ErrorAction SilentlyContinue | ForEach-Object { To-Rel $_.FullName }
    }
}

# Compose contents
$treeContent = @()
$treeContent += "Project tree snapshot ($date)"
$treeContent += ""
$treeContent += "Top-level directories:" 
$treeContent += ($topDirs | ForEach-Object { "- $_" })
$treeContent += ""
$treeContent += "Top-level files:" 
$treeContent += ($topFiles | ForEach-Object { "- $_" })
$treeContent += ""
$treeContent += "Key source files (src):"
$treeContent += ($srcFiles | Sort-Object | ForEach-Object { "- $_" })
$treeContent += ""
$treeContent += "Grammar files:"
$treeContent += ($grammarFiles | Sort-Object | ForEach-Object { "- $_" })
$treeContent = ($treeContent -join "`n") + "`n"

$invContent = @()
$invContent += "Files of interest snapshot ($date)"
$invContent += ""
$invContent += "Key files:"
$invContent += ($keyFilesList | ForEach-Object { "- $_" })
$invContent += ""
$invContent += "Executables:"
$invContent += (($executables | Sort-Object) | ForEach-Object { "- $_" })
$invContent += ""
$invContent += "Counts:"
$srcCount = 0
if ($null -ne $srcFiles) { $srcCount = @($srcFiles).Count }
$gramCount = 0
if ($null -ne $grammarFiles) { $gramCount = @($grammarFiles).Count }
$invContent += "- src files: $srcCount"
$invContent += "- grammar files: $gramCount"
$invContent = ($invContent -join "`n") + "`n"

$overviewContent = @()
$overviewContent += "ClearLanguage overview (auto, $date)"
$overviewContent += ""
$overviewContent += "Stack: ANTLR4 (grammar at grammar/ClearLanguage.g4), LLVM, CMake + Presets, Conan, C++17"
$overviewContent += "Entrypoint: src/main.cpp"
$overviewContent += "Build outputs: build/, build-ninja/"
$overviewContent += "Executables present: $($executables.Count)"
$overviewContent = ($overviewContent -join "`n") + "`n"

# Write 'latest' memories (stable names) and also a dated snapshot
Write-Utf8 (Join-Path $memDir 'project_tree_latest.md') $treeContent
Write-Utf8 (Join-Path $memDir "project_tree_$date.md") $treeContent

Write-Utf8 (Join-Path $memDir 'project_inventory_latest.md') $invContent
Write-Utf8 (Join-Path $memDir "project_inventory_$date.md") $invContent

Write-Utf8 (Join-Path $memDir 'project_overview_latest.md') $overviewContent
Write-Utf8 (Join-Path $memDir "project_overview_$date.md") $overviewContent

Write-Host "Serena snapshots updated: project_*_{latest,$date}.md"
