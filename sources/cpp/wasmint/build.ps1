param($Tool, [switch]$Clean, [switch]$VerboseBuild, $Launcher)


if ($Launcher -ne $null) {
    $DCMAKE_C_COMPILER_LAUNCHER = "-DCMAKE_C_COMPILER_LAUNCHER=$Launcher"
    $DCMAKE_CXX_COMPILER_LAUNCHER = "-DCMAKE_CXX_COMPILER_LAUNCHER=$Launcher"
}

if ($Clean.IsPresent) {
    rm .Build -Force -Recurse -EA SilentlyContinue | Out-Null
}
mkdir -Force .build | Out-Null
pushd .build

$Generator = "Ninja"

if ($Tool -eq $null) { $Tool = "clang" }

Write-Verbose "Tool = [$Tool]"

if ($Tool.ToLower() -eq "clang") {
    $clang = get-command clang
    if ($clang.Definition -ne $null) {
        Write-Verbose "Clang found at:"
        Write-Verbose "clang: $((get-command clang).Definition)"
        Write-Verbose "clang: $((get-command clang++).Definition)"
        $env:CC = (get-command clang).Definition
        $env:CXX = (get-command clang++).Definition
        if ((get-command Ninja) -ne $null) {
            $Generator = "Ninja"
        }
    }
}
if ($Tool.ToLower() -eq "msvc") {
    $Generator = "Visual Studio 16 2019"
}

$DCMAKE_VERBOSE_MAKEFILE = ""
if ($VerboseBuild.IsPresent) {
    $DCMAKE_VERBOSE_MAKEFILE = "-DCMAKE_VERBOSE_MAKEFILE:BOOL=ON"
}

Write-Verbose "cmake  -DCMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=TRUE -DBUILD_SHARED_LIBS=TRUE $DCMAKE_VERBOSE_MAKEFILE $DCMAKE_C_COMPILER_LAUNCHER $DCMAKE_CXX_COMPILER_LAUNCHER -G $Generator .."

set-alias cmake 'C:\Program Files\CMake\bin\cmake.exe'
cmake  -DCMAKE_WINDOWS_EXPORT_ALL_SYMBOLS=TRUE -DBUILD_SHARED_LIBS=TRUE $DCMAKE_VERBOSE_MAKEFILE $DCMAKE_C_COMPILER_LAUNCHER $DCMAKE_CXX_COMPILER_LAUNCHER -G $Generator ..
cmake --build .
popd