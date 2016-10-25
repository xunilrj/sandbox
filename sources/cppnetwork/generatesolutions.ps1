param([switch]$Install, [switch]$Generate, [switch]$BuildDebug)

Push-Location
cd (Split-Path $PSCommandPath -Parent)

if($Install.IsPresent){
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

if($Generate.IsPresent){
    Write-Information "Generating..."
    rm .\.build -Recurse -Force -EA SilentlyContinue
    mkdir .build | cd
    mkdir msvc | cd
    cmake ..\.. -G "Visual Studio 14 2015 Win64"
    Write-Information "Finished!"
}

if($BuildDebug.IsPresent){
    Write-Information "Building..."
    cmake --build .\.build\msvc --config Debug
    Write-Information "Finished!"
}
Pop-Location