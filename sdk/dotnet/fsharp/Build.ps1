# `Build.ps1 Pack Build` to create the nugets

[CmdletBinding()]
param (
    [Parameter(HelpMessage="The action to execute.")]
    [ValidateSet("Build", "Pack" )]  # ,"Test"
    [string] $Action = "Build",

    [Parameter(HelpMessage="The msbuild configuration to use.")]
    [ValidateSet("Debug", "Release")]
    [string] $Configuration = "Debug",

    [switch] $SkipClean
)

function RunCommand {
    param ([string] $CommandExpr)
    Write-Verbose "  $CommandExpr"
    Invoke-Expression $CommandExpr
}

$rootDir = $PSScriptRoot
$srcDir = Join-Path -Path $rootDir -ChildPath 'src'
# $testDir = Join-Path -Path $rootDir -ChildPath 'test'

switch ($Action) {
    # "Test"        { $projectDir = $srcDir }
    "Pack"        { $projectDir = $srcDir }
    Default       { $projectDir = $srcDir }
}

if (!$SkipClean.IsPresent)
{
    RunCommand "dotnet restore $projectDir --force --force-evaluate --nologo --verbosity quiet"
    RunCommand "dotnet clean $projectDir -c $Configuration --nologo --verbosity quiet"
}

switch ($Action) {
    #"Test"        { RunCommand "dotnet test `"$projectDir`"" }
    "Pack"        { RunCommand "dotnet pack `"$projectDir`" -c $Configuration --include-symbols --include-source" }
    Default       { RunCommand "dotnet build `"$projectDir`" -c $Configuration" }
}
