global _start

section .text

_write_from_memory:
  mov rdi, [rax + 16]
  mov rsi, [rax + 8]
  mov rdx, [rax + 0]
_write: ; rdi (STDOUT_FILENO=1), rsi=const void *buf, rdx=count
  mov rax, 1
  syscall
  ret
_start:
  mov rax, 1        ; write(
  mov rdi, 1        ;   STDOUT_FILENO,
  mov rsi, msg      ;   "Hello, world!\n",
  mov rdx, msglen   ;   sizeof("Hello, world!\n")
  syscall           ; );

  push qword 1
  push qword msg  
  push qword msglen
  mov rax, rsp
  call _write_from_memory

  mov rax, 41       ; s = socket(
  mov rdi, 2        ;   AF_INET = 2,
  mov rsi, 2        ;   SOCK_STREAM	= 2,
  mov rdx, 0        ;   0
  syscall           ; )

  cmp rax, 0
  jl error
  
  mov rdi, rax      ; sockfd = s
  mov rax, 41       ; shutdown(  
  mov rsi, 0        ;   how,
  syscall           ; )

  mov rax, 60       ; exit(
  mov rdi, 0        ;   EXIT_SUCCESS
  syscall           ; );

error:
  mov rax, 60       ; exit(
  mov rdi, 1        ;   1
  syscall           ; );

section .rodata
  msg: db "Hello, world!", 10
  msglen: equ $ - msg