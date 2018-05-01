param([Switch]$Clean)

$currentDir = Split-Path $PSCommandPath -Parent
pushd $currentDir

$binFolder = "build"

set-alias vcpkg C:\github\vcpkg\vcpkg.exe
vcpkg install catch2:x64-windows-static

if($Clean.IsPresent)
{
    rm $binFolder -Recurse -Force
}

pushd
if([System.IO.Directory]::Exists($binFolder) -eq $false)
{
    mkdir $binFolder -EA SilentlyContinue
    cd $binFolder
    cmake .. -G "Visual Studio 15 2017 Win64" -DCMAKE_TOOLCHAIN_FILE=C:\github\vcpkg\scripts\buildsystems\vcpkg.cmake -DVCPKG_TARGET_TRIPLET=x64-windows-static
    cd ..
}
cd $binFolder
cmake --build .

popd