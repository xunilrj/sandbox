param([switch]$SkipInstall, [switch]$SkipGenerate, [string]$Build = "Debug", [string]$Run = "Debug", [switch]$SkipRun)

Push-Location
cd (Split-Path $PSCommandPath -Parent)

if($SkipInstall.IsPresent -eq $false){
    Write-Information "Installing..."
    $choco = command choco -EA SilentlyContinue
    if($choco -eq $null){
        iwr https://chocolatey.org/install.ps1 -UseBasicParsing -Verbose | iex -Verbose
    }

    $cmake = command cmake -EA SilentlyContinue
    if($cmake -eq $null){
        choco install cmake -y -v
    }
    Write-Information "Finished!"
}

if($SkipGenerate.IsPresent -eq $false){
    Write-Information "Generating..."
    rm .\.build -Recurse -Force -EA SilentlyContinue
    mkdir .build | cd
    mkdir msvc | cd
    cmake ..\.. -G "Visual Studio 14 2015 Win64"
    Write-Information "Finished!"
}

if([System.String]::IsNullOrEmpty($Build) -eq $false){
    Write-Information "Building..."
    cmake --build .\.build\msvc --config $Build
    Write-Information "Finished!"
}

if($SkipRun -eq $false -and [System.String]::IsNullOrEmpty($Run) -eq $false){
    start ".\.build\msvc\$Run\input.exe"
}

Pop-Location
