Import-Module C:\tools\llvm\llvm.psm1 -Force -DisableNameChecking
Set-llvm C:\tools\llvm\LLVM-6.0.0-r320423-win64

clang++ -g -std=c++1z  ./sources/main.cpp