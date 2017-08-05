nasm -f win64 simple.asm
link /subsystem:console /nodefaultlib /entry:WinMain simple.obj kernel32.lib user32.lib /largeaddressaware:no