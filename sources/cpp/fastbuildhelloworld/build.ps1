Push-Location (Split-Path $PSCommandPath -Parent)

set-alias cl "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\cl.exe"
set-alias dumpbin "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\dumpbin.exe"
set-alias link "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\link.exe"

cl main.cpp /MT /c /I "C:\Program Files (x86)\Windows Kits\10\Include\10.0.14393.0\shared" /I "C:\Program Files (x86)\Windows Kits\10\Include\10.0.14393.0\ucrt" /I "C:\Program Files (x86)\Windows Kits\10\Include\10.0.14393.0\um" /I "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\include"
dumpbin .\main.obj /DISASM /OUT:main.asm.txt
link main.obj /ENTRY:main /SUBSYSTEM:WINDOWS /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.14393.0\ucrt\x64" /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.14393.0\um\x64" /LIBPATH:"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\lib\amd64" libucrt.lib kernel32.lib Advapi32.lib

Pop-Location