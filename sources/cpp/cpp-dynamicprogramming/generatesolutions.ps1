param([switch]$Install, [switch]$Generate, [switch]$Build, [string]$Arch = "64", [string]$Config = "Debug")

$InformationPreference = "Continue"

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
    Write-Information "Removing old build..."
    rm .\.build -Recurse -Force

    Write-Information "Generating x86..."    
    mkdir .build | cd
    mkdir msvc86 | cd
    cmake ..\.. -G "Visual Studio 14 2015"

    Write-Information "Generating x64..."
    cd ..   
    mkdir msvc64 | cd
    cmake ..\.. -G "Visual Studio 14 2015 Win64"

    Write-Information "Finished!"
}

if($Build.IsPresent){
    Write-Information "Building..."
    $textResult = (cmake --build ".\.build\msvc$Arch" --config $Config *>&1)
    
    if($LASTEXITCODE -gt 0){
        Write-Information ([System.String]::Join([System.Environment]::NewLine,$textResult))
        Write-Error "FAILED!"
        exit 1
    }else{
        Write-Information "Finished!"
    }    
}
Pop-Location