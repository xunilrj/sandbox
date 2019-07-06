Import-Module C:\tools\llvm\llvm.psm1 -Force -DisableNameChecking
Set-llvm C:\tools\llvm\v8.0.0

clang++ -g -std=c++1z ./sources/main.cpp