rm .\helloworld.exe
nasm -fwin32 .\helloworld.asm
. ../vs.ps1
link /subsystem:console /nodefaultlib /entry:main helloworld.obj kernel32.lib /LIBPATH:"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.16299.0\um\x86" /DEBUG:FULL