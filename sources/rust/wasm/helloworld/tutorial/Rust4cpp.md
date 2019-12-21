---
marp: true
---

# Rust for C++
###### and why it improves your C++ code

Daniel Frederico Lins Leite

---
# ASM and the Processor

---
# Calling functions

- simple function
- return by value
- return by pointer/reference

---

![alt text](https://techreport.com/r.x/64-bits/register-diagram.gif "Stack Pointer")

- RAX = 64 bits
- EAX = lower 32 bits

- MOV RAX, 1
-- Move value 1 to RAX register
- MOV [RAX], 2
-- Move value 2 to memory indexed by RAX register value

---

# Simple function

```c++
int f(int a)
{
    return a*3;
}

int main()
{
    return f(1)*3;
}
```
- Parameter "a" using RDI register
- Return value using RAX register

https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention

---


```asm
f(int):
        push    rbp
        mov     rbp, rsp        
        mov     dword ptr [rbp - 4], edi //mem[rdp-4] = edi
        imul    eax, dword ptr [rbp - 4], 3 //eax = mem[rdp-4] * 3
        pop     rbp
        ret
main:                            
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     dword ptr [rbp - 4], 0
        mov     edi, 1
        call    f(int)
        imul    eax, eax, 3
        add     rsp, 16
        pop     rbp
        ret
```
https://godbolt.org/z/2VqBO5

---

# Return by value

```c++
struct Data {int a; long b; float c;};
Data f(int a)
{
    return {a*3,a*6,(float)a*9};
}

int main()
{
    auto data = f(1);
    return data.a+data.b+data.c;
}
```

---

```asm
main:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 32
        mov     dword ptr [rbp - 4], 0      
        lea     rdi, [rbp - 32]             // rdi = rbp - 32
        mov     esi, 1                      // rsi = 1
        call    f(int)                      // f(rdi, rsi)
        movsxd  rax, dword ptr [rbp - 32]   // rax = mem[rbp - 32]
        add     rax, qword ptr [rbp - 24]   // rax += mem[rbp - 24]
        cvtsi2ss        xmm0, rax           // xmm0 = (float)rax
        addss   xmm0, dword ptr [rbp - 16]  // xmm0 += mem[rpb-16]
        cvttss2si       eax, xmm0           // eax = (int)xmm0
        add     rsp, 32
        pop     rbp
        ret
```

---

```asm
.LCPI0_0:
        .long   1091567616                          // LCPI0_0 = (float)9
                                                    // Data f(int a) 
                                                    // void f(Data&, int)
f(int):                                             // RAX f(RDI, RSI)
        push    rbp
        mov     rbp, rsp
        mov     rax, rdi                            // rax = rdi
        movss   xmm0, dword ptr [rip + .LCPI0_0]    // xmm0 = 9
        mov     dword ptr [rbp - 4], esi            // mem[rbp-4] = rsi
        imul    ecx, dword ptr [rbp - 4], 3         // rcx = mem[rbp-4] * 3
        mov     dword ptr [rdi], ecx                // mem[rdi] = rcx
        imul    ecx, dword ptr [rbp - 4], 6         // rcx = mem[rbp-4] * 6
        movsxd  rdx, ecx                            // rdx = rcx
        mov     qword ptr [rdi + 8], rdx            // mem[rdi+8] = rdx
        cvtsi2ss        xmm1, dword ptr [rbp - 4]   // xmm1 = (float)mem[rbp-4]
        mulss   xmm1, xmm0                          // xmm1 *= xmm0
        movss   dword ptr [rdi + 16], xmm1          // mem[rdi+16] = xmm1
        pop     rbp
        ret
```

---

```c++
struct Data {int a; long b; float c;};
Data* f(int a)
{
    return new Data{a*3,a*6,(float)a*9};
}

int main()
{
    auto* data = f(1);
    return data->a+data->b+data->c;
}
```

---

```asm
main:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     dword ptr [rbp - 4], 0      // mem[rbp-4] = 0
        mov     edi, 1                      // rdi = 1
        call    f(int)                      // f(rdi)
        mov     qword ptr [rbp - 16], rax   // mem[rbp-16] = rax
        mov     rax, qword ptr [rbp - 16]   // rax = mem[rbp-16]
        movsxd  rax, dword ptr [rax]        // rax = mem[rax]
        mov     rcx, qword ptr [rbp - 16]   // rcx = mem[rbp-16]
        add     rax, qword ptr [rcx + 8]    // rax = mem[rcx + 8]
        cvtsi2ss        xmm0, rax           // xmm0 = (float)rax
        mov     rax, qword ptr [rbp - 16]   // rax = mem[rbp-16]
        addss   xmm0, dword ptr [rax + 16]  // xmm0 = mem[rax+16]
        cvttss2si       eax, xmm0           // eax = (int)xmm0
        add     rsp, 16
        pop     rbp
        ret
```

---

```asm
.LCPI0_0:
        .long   1091567616                          //LCPI0_0 = 9
                                                    // Data* f(int a)
                                                    // RAX f(RDI)
f(int):
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     dword ptr [rbp - 4], edi            //mem[rbp-4] = rdi
        mov     edi, 24                             //rdi = 24
        call    operator new(unsigned long)         //rax = new(rdi)
        movss   xmm0, dword ptr [rip + .LCPI0_0]    //xmm0 = 9
        mov     rcx, rax                            //rcx = rax
        imul    edx, dword ptr [rbp - 4], 3         //rdx = mem[rbp-4] * 3
        mov     dword ptr [rax], edx                //mem[rax] = rdx
        imul    edx, dword ptr [rbp - 4], 6         //rdx = mem[rbp-4] * 6
        movsxd  rsi, edx                            //rsi = rdx
        mov     qword ptr [rax + 8], rsi            //mem[rax+8] = rsi
        cvtsi2ss        xmm1, dword ptr [rbp - 4]   //xmm1 = (float)mem[rbp-4]
        mulss   xmm1, xmm0                          //xmm1 *= xmm0
        movss   dword ptr [rax + 16], xmm1          //mem[rax+16] = (int)xmm1
        mov     rax, rcx                            //rax = rcx
        add     rsp, 16
        pop     rbp
        ret
```

---
# C++ common problems - return ref to local

```c++
struct Data {int a; long b; float c;};
Data& f(int a)
{
    auto d = Data{a*3,a*6,(float)a*9};
    return d;
}
```

- Returning address of object that no longer exist
- Newer compilers generate error/warning

---

```asm
.LCPI0_0:
        .long   1091567616                          // LCPI0_0 = 9
                                                    // Data& f(int a)
                                                    // RAX f(RDI)
f(int):
        push    rbp
        mov     rbp, rsp
        movss   xmm0, dword ptr [rip + .LCPI0_0]    //xmm0 = 9
        mov     dword ptr [rbp - 4], edi            //mem[rbp-4] = rdi 
        imul    eax, dword ptr [rbp - 4], 3         //rax = mem[rbp-4] * 3
        mov     dword ptr [rbp - 32], eax           //mem[rbp-32] = rax
        imul    eax, dword ptr [rbp - 4], 6         //rax = mem[rbp-4] * 6
        movsxd  rcx, eax                            //rcx = rax
        mov     qword ptr [rbp - 24], rcx           //mem[rbp-24] = rcx
        cvtsi2ss        xmm1, dword ptr [rbp - 4]   //xmm1 = (float)mem[rbp-4]
        mulss   xmm1, xmm0                          //xmm1 *= xmm0
        movss   dword ptr [rbp - 16], xmm1          //mem[rbp-16] = xmm1
        lea     rax, [rbp - 32]                     //rax = rbp-32
        pop     rbp
        ret
```

---

# The Stack

![alt text](https://eli.thegreenplace.net/images/2011/08/x64_frame_nonleaf.png "Stack")

https://eli.thegreenplace.net/2011/02/04/where-the-top-of-the-stack-is-on-x86/
https://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64

---

# Why return &

If returning by ref CAN cause problems. And return by pointer NEVER cause problem. Why one would choose it?

---

# First

You can return a pointer to a local. But let us ignore this case because it is pretty much the same as returning "ref to local".

```c++
struct Data {int a; long b; float c;};
Data* f(int a)
{
    auto d = Data{a*3,a*6,(float)a*9};
    return &d;
}
```

---

The real problem with returning by pointer is that the deconstructor is never called. For example:

```c++
#include <iostream>
class Data
{
    int data;
    public:
    Data(int a) : data{a} {}
    ~Data()
    {
        std::cout << "~Data()" << std::endl;
    }
};
Data f(int a) { auto data = Data{a}; return data; }
int main()
{
    auto data = f(3);
    return 0;
}
```

---

What is relevant:
- Object has a constructor
- Object has a deconstructor

---

Constructor is just a normal function

```asm
                                        // Data::Data(int)
                                        // Data::Data(Data&, int)
Data::Data(int):                        // Data::Data(rdi, rsi)
        push    rbp
        mov     rbp, rsp
        mov     QWORD PTR [rbp-8], rdi
        mov     DWORD PTR [rbp-12], esi
        mov     rax, QWORD PTR [rbp-8]
        mov     edx, DWORD PTR [rbp-12]
        mov     DWORD PTR [rax], edx
        nop
        pop     rbp
        ret
```

---

as the destructor:

```asm
.LC0:
        .string "~Data()"
                                        // Data::~Data()
                                        // Data::~Data(Data&)
Data::~Data() [base object destructor]: // RAX Data::~Data(RDI)
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     QWORD PTR [rbp-8], rdi
        mov     esi, OFFSET .LC0
        mov     edi, OFFSET COUT
        call    operator<<
        mov     esi, OFFSET .ENDLINE
        mov     rdi, rax
        call    operator<<
        nop
        leave
        ret
```

Important to notice that the destructor does not deallocate memory. It is just a function that is automatically called at certain points.

---

```asm
f(int):
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     QWORD PTR [rbp-8], rdi
        mov     DWORD PTR [rbp-12], esi
        mov     edx, DWORD PTR [rbp-12]
        mov     rax, QWORD PTR [rbp-8]
        mov     esi, edx
        mov     rdi, rax
        call    Data::Data(int)         // Data::Data(rdi, rsi)
        nop
        mov     rax, QWORD PTR [rbp-8]
        leave
        ret
```

The first surprise here is that "f" does not call the object destructor. Why?
 
---

## Mandatory elision of copy/move operations
Under the following circumstances, the compilers are required to omit [...] constructor[s] and [...] destructor[s], [even when they] have observable side-effects. The objects are constructed directly into the storage where they would otherwise be copied [...] to.
https://en.cppreference.com/w/cpp/language/copy_elision

---

But the destructor is called inside the main function, as expected.

```asm
main:
        push    rbp
        mov     rbp, rsp
        push    rbx
        sub     rsp, 24
        lea     rax, [rbp-20]   //rax = rbp - 20
        mov     esi, 3          //rsi = 3
        mov     rdi, rax        //rdi = rax
        call    f(int)          //f(rdi, rsi)
        mov     ebx, 0          //rbx = 0
        lea     rax, [rbp-20]   //rax = rbp - 20
        mov     rdi, rax        //rdi = rax
        call    Data::~Data()   //Data::~Data(rdi)
        mov     eax, ebx
        add     rsp, 24
        pop     rbx
        pop     rbp
        ret
```

---

The problem is that if we change this to return pointer, the deconstructor is not called. But first let us cahnge to return the reference.

Now we are making two mistakes. That are easily seen in the assembly.

```c++
Data& f(int a) { auto data = Data{a}; return data; }
int main()
{
    auto data = f(3);
    return 0;
}
```

---

```asm
f(int):
        push    rbp
        mov     rbp, rsp
        push    rbx
        sub     rsp, 40
        mov     DWORD PTR [rbp-36], edi
        mov     edx, DWORD PTR [rbp-36]
        lea     rax, [rbp-20]
        mov     esi, edx
        mov     rdi, rax
        call    Data::Data(int)
        mov     ebx, 0
        lea     rax, [rbp-20]
        mov     rdi, rax
        call    Data::~Data() [complete object destructor]
        mov     rax, rbx
        add     rsp, 40
        pop     rbx
        pop     rbp
        ret
```

First we are returning a reference to a local variable. No "copy elision" was done in this case, so 

---

```asm
main:
        push    rbp
        mov     rbp, rsp
        push    rbx
        sub     rsp, 24
        mov     edi, 3                      //rdi=3
        call    f(int)                      //f(rdi)
        mov     eax, DWORD PTR [rax]        //rax = mem[rax]
        mov     DWORD PTR [rbp-20], eax     //mem[rbp-20] = rax
        mov     ebx, 0
        lea     rax, [rbp-20]               //rax = rbp-20
        mov     rdi, rax                    //rdi = rax
        call    Data::~Data()               //Data::~Data(rdi)
        mov     eax, ebx
        add     rsp, 24
        pop     rbx
        pop     rbp
        ret
```

It is clear that in this case, RAX was the address to struct, we copied the data to a local (inside main) and then called the constructor.

---

This example created a strange fact. The destructor was called twice, but constructor only once. Why?

The truth is that the constructor was also copied twice. Not the standard constructor, but the "copy constructor". The real code is:

```c++
Data& f(int a) 
{
    auto data = Data::Data(a);
    Data::~Data(data);
    return data; 
}
int main()
{
    auto& temp = f(3)
    auto data = Data::Data(temp);
    Data::~Data(data);
    return 0;
}
```

---

Now our two mistakes are clear.
- First, we returned a reference to address that is not valid anymore (ref to local);
- Second, we are copying a already destructed object;

---

We can solve them returning a pointer

---

In this case, the main is much simples. We just copy the pointer, and not the data.

```c++
main:
        push    rbp
        mov     rbp, rsp
        sub     rsp, 16
        mov     edi, 3                  //rdi = 3
        call    f(int)                  //rax = f(rdi)
        mov     QWORD PTR [rbp-8], rax  //mem[rbp-8] = rax
        mov     eax, 0
        leave
        ret
```

---

```asm
f(int):
        push    rbp
        mov     rbp, rsp
        push    rbx
        sub     rsp, 40
        mov     DWORD PTR [rbp-36], edi
        mov     edi, 4                      //rdi=4
        call    operator new(unsigned long) //rax = new(rdi)
        mov     rbx, rax                    //rbx = rax
        mov     eax, DWORD PTR [rbp-36]     //rax = mem[rbp-36]
        mov     esi, eax                    //rsi = rax
        mov     rdi, rbx                    //rdi = rbx
        call    Data::Data(int)             //Data::Data(rdi, rsi)
        mov     QWORD PTR [rbp-24], rbx
        mov     rax, QWORD PTR [rbp-24]     //rax = mem[rbp-24]
        add     rsp, 40
        pop     rbx
        pop     rbp
        ret
```

---

Importante here:
1 - the "new operator" is just another normal function. It receives the size to allocate and return the memory address;
2 - the destructor is never called;
3 - we never deallocated the memory;

We fixed two problems, and created two new.

---

Before fixing these problems. Let us sum up:

1 - Returning by value does not generate problems;
1.1 - But copying "big" structs can be take a lot of time;
2 - Returning by reference is always fast;
2.1 - but when we return by ref we must guarantee that the object is valid when the reference is used;
3 - returning by pointer is always fast;
3.1 - but we need to allocate memory in the heap;
3.1.1 - this can take a lot of time;
3.1.2 - the time is not necessarily constant. The first allocation can be quick, the 1000th allocation can take a lot of more time.
3.2 - we need to deallocate the memory manually;
3.3 - the destructor is not called (will be by the deallocation);

---

The CPP Core Guidelines C61 states:

"Note Prefer value semantics unless you are building a “smart pointer”. Value semantics is the simplest to reason about and what the standard-library facilities expect."
https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#c61-a-copy-operation-should-copy