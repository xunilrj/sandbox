Import-Module c:\tools\llvm\llvm.psm1 -Force
Set-LLVM C:\tools\llvm\debug-x86_64-pc-windows-msvc\            

$libsdir = ''
$libs = ""
-split @"
clang++ kaleidoscope001.cpp -o kaleidoscope001.exe
--include-directory C:\github\wasm-toolchain\llvm\include
--include-directory C:\tools\llvm\include
-L C:\tools\llvm\debug-x86_64-pc-windows-msvc\lib
"@ -join " " | iex