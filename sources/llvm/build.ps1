Import-Module c:\tools\llvm\llvm.psm1 -Force
Set-LLVM C:\tools\llvm\debug-x86_64-pc-windows-msvc\            

$libsdir = ''
$libs = ""
@"
clang++ main.cpp -S -emit-llvm -m64
llc main.ll -march=x86-64 -filetype=obj
lld-link main.obj /MACHINE:X64 $libsdir $libs
/libpath:"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\lib\amd64"
libcmt.lib
"@ -split "`n" -join " " | iex