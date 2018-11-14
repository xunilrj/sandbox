1..100|%{Write-Host ""}
cls
Write-Host "Compiling..."
Import-Module C:\tools\llvm\llvm.psm1 -Force -DisableNameChecking
Set-llvm C:\tools\llvm\LLVM-6.0.0-r320423-win64

-split @"
clang++
-g -v
-std=c++1z -stdlib=libc++
-I"C:\github\wasm-toolchain\llvm\projects\libcxx"
-fcoroutines-ts
.\main.all.cpp
-o main.all.exe
"@ -join " " | iex

Write-Host "Done."
