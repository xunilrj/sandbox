global _start
_start:
    ; Write.
    mov rdi, 1                  ; write to stdout
    mov rsi, hello_world        ; use string at hello_world
    mov rdx, hello_world_len    ; size = hello_world_len
    mov rax, 1                  ; use the write syscall
    syscall

    ; Exit.
    mov rax, 60                 ; use the _exit syscall
    mov rdi, 0                  ; error code 0
    syscall

hello_world db "hello world", 10
hello_world_len equ $ - hello_world