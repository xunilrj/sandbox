Push-Location
cd (Split-Path $PSCommandPath -Parent)

$choco = command choco -EA SilentlyContinue
if($choco -eq $null){
    iwr https://chocolatey.org/install.ps1 -UseBasicParsing -Verbose | iex -Verbose
}

$cmake = command cmake -EA SilentlyContinue
if($cmake -eq $null){
    choco install cmake -y -v
}

rm .\.build -Recurse -Force -EA SilentlyContinue
mkdir .build | cd
mkdir msvc | cd
cmake ..\.. -G "Visual Studio 14 2015 Win64"
Pop-Location