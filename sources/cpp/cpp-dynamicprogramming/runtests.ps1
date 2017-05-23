./generatesolutions.ps1 -Build

if($LASTEXITCODE -eq 0 ) {
    #OpenCppCoverage --sources ./sources -- ./.build/msvc64/Debug/eulerproblems.exe
    Write-Information "Running..."
     ./.build/msvc64/Debug/cppdynamic.exe
}
