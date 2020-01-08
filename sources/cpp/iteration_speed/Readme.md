# ECS - System Design Choices

In a ECS architecture, Systems can work on any arbitrary set of entities. And entities can have arbitrary set of components. And components are store in arbitrary data structures, although we prefer packed ones like std:vector.

Here we will assess the performance impact of some possible design choices. First we will test the performance of transversing some data structures. We will call this "linear runs". Then we will access the items inside this structure following another structure.

The last part we dive deep into the generated ASM code to assess if the solution will be as optimized as we need.

# Understanding Iterator Performance

Analyzing our performance in this example, we can see that a liner run is very fast for std::vector.

First we will try multiple way to run a vector.

```c++
int accum = 0;

std::vector<int> numbers (1000000);
std::fill(numbers.begin(), numbers.end(), 1);

std::cout << "------------------------Linear run - vector" << std::endl;
accum = 0; time_for_each (numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;
accum = 0; time_begin_end(numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;
accum = 0; time_by_index (numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;
accum = 0; time_data_directly(numbers, [&](auto&& x){ accum += x; }); std::cout << " accum: " << accum << std::endl;
```

We can see from the results that, it does not matter much how we access.

```
------------------------Linear run - vector
For Each: 0.0674161ms accum: 1000000000
Begin/End: 0.0662968ms accum: 1000000000
Indices: 0.0664087ms accum: 1000000000
Data: 0.068697ms accum: 1000000000
```

But, of course, that is not what we intend to do. We want to update only a subset of this vector. So the first approach is to use a set to store the ids of the entities that we want to update and access them by index in the vector. 

```c++
std::cout << "------------------------From sorted Set" << std::endl;
std::set<size_t> selected;
uint64_t last = 0;
for(int i = 0; i < 1000000; ++i)
{
selected.insert(last);
++last;
}

accum = 0; time_foreach_index(selected, numbers, [&](auto&& x){ accum += x; }, 10); std::cout << " accum: " << accum << std::endl;
accum = 0; time_iterators_iterators(numbers, selected, [&](auto&& x){ accum += x; }, 10); std::cout << " accum: " << accum << std::endl;
```

But the result is disastrous:

```
------------------------From sorted Set
ForEach/Index: 20.7487ms accum: 10000000
It/It: 21.237ms accum: 10000000
```

The problem here is that std::set is implemented as red-black tree. So just transversing the tree is much slower. To test this we will transverse just the std::set.

```c++
std::cout << "------------------------Linear run - set" << std::endl;

accum = 0; time_for_each(selected, [&](auto&& x){ accum += x; }, 10); std::cout << " accum: " << accum << std::endl;
```

We can see that just transversing the std::set takes A LOT MORE than transversing the whole std::vector.

```c++
------------------------Linear run - set
For Each: 19.9023ms accum: 653067456
```

This result surprise some, but the point here is that, this was not the use case envisioned when stD::set was built. We are using it here just because we want a "bag" of integers that do not repeat. But std::set is good to test existance inside this "bag". Any other use will probably be sub-optimal, as it is in our case.

Why you shouldn't use set (and what you should use instead) - Matt Austern  
http://lafstern.org/matt/col1.pdf  

Truth is, the "std" does not have the collection that we want in this case. We want something that avoids duplication, keep its element sorted and it is fast to do a "linear run". We all know that there is no free-lunch. We will have to accept bad complexity/performance in some case:

- We will be doind a "linear run" on every frame of the game, so this is the main case: std::vector is the campion here;
- In the vast majority of cases we will be appending the sorted list. The "id" will be the greatest;
- We accept "bad" complexity/performance for insertion of small ids;

In this scenario we will build something custom, a "container adaptor" as they are known in c++. It is just a couple of modification on top of the std::vector.

```c++
template <typename T, class TCompare = std::less<T>>
class sorted_vector
{
public:
    using iterator = typename std::vector<T>::iterator;
    using const_iterator = typename std::vector<T>::const_iterator;
 
    iterator begin() { return data.begin(); }
    iterator end() { return data.end(); }
    const_iterator begin() const { return data.begin(); }
    const_iterator end() const { return data.end(); }

    const_iterator cbegin() { return data.cbegin(); }
    const_iterator cend() { return data.cend(); }

    iterator insert(const T& t) 
    {
        iterator i = std::lower_bound(begin(), end(), t, cmp);
        if (i == end() || cmp(t, *i))
            data.insert(i, t);
        return i;
    }
private:
    TCompare cmp;
    std::vector<T> data;
};
```

This will keep a sorted vector, sorted as we insert items. We avoid duplicated by the "cmp(t, *it)".

```c++
std::cout << "------------------------From sorted vector" << std::endl;
sorted_vector<size_t> selected2;
last = 0;
for(int i = 0; i < 1000000; ++i)
{
        selected2.insert(last);
        ++last;
}

accum = 0; time_foreach_index(selected2, numbers, [&](auto&& x){ accum += x; }); std::cout << accum << std::endl;
```

Now we take a LOT less than using the set. And this is the absolutely worst case where we have all entities.

```
------------------------From sorted vector
ForEach/Index: 0.745353ms1000000000
```

With this we are ready to start working on joins.

# Understanding Joins

# Understanding Iterator Code

Today we have multiple ways to iterate a simple std::vector. Here we will analyze the generated assembly and understand which would be the prefered way. 

Let us start simple. We have a std::vector initialized like this:

```c++
std::vector<int> items (100000000);
std::fill(items.begin(), items.end(), 1);
```

Everything between the code below will have its execution time measured.

```c++
deltaTime<double> dt{ true };
...
auto elapsed = dt.elapsed();
```

## Accessing the data directly

First, we will access the data directly like a plain pointer.

```c++
template <typename T>
void time_data_directly(const T& items)
{
    typename T::value_type accum = 0;
    auto size = items.size();
    auto* item = items.data();

    deltaTime<double> dt{ true };
    for(int i = 0; i < size; ++i, ++item)
    {
        accum += *item;
    }
    auto elapsed = dt.elapsed();
    std::cout << "Data: " << elapsed << "ms. [" << accum << "]" << std::endl;
}
```

And now the generated ASM in -O3. It is going to be a little bit scary. But let us slice it and see what we have.


```asm
void time_data_directly:
        push    r14
        push    rbx
        push    rax
        mov     rax, qword ptr [rdi]
        mov     rcx, qword ptr [rdi + 8]
        sub     rcx, rax
        je      .LBB4_1
        sar     rcx, 2
        cmp     rcx, 1
        mov     r8d, 1
        cmova   r8, rcx
        cmp     r8, 7
        ja      .LBB4_7
        xor     edx, edx
        xor     ebx, ebx
        jmp     .LBB4_4
.LBB4_1:
        xor     ebx, ebx
        jmp     .LBB4_5
.LBB4_7:
        mov     rdx, r8
        and     rdx, -8
        lea     rbx, [rdx - 8]
        mov     rsi, rbx
        shr     rsi, 3
        add     rsi, 1
        mov     edi, esi
        and     edi, 3
        cmp     rbx, 24
        jae     .LBB4_9
        pxor    xmm0, xmm0
        xor     ebx, ebx
        pxor    xmm1, xmm1
        test    rdi, rdi
        jne     .LBB4_12
        jmp     .LBB4_14
.LBB4_9:
        mov     ebx, 1
        sub     rbx, rsi
        lea     rsi, [rdi + rbx]
        add     rsi, -1
        pxor    xmm0, xmm0
        xor     ebx, ebx
        pxor    xmm1, xmm1
.LBB4_10:                               # =>This Inner Loop Header: Depth=1
        movdqu  xmm2, xmmword ptr [rax + 4*rbx]
        paddd   xmm2, xmm0
        movdqu  xmm0, xmmword ptr [rax + 4*rbx + 16]
        paddd   xmm0, xmm1
        movdqu  xmm1, xmmword ptr [rax + 4*rbx + 32]
        movdqu  xmm3, xmmword ptr [rax + 4*rbx + 48]
        movdqu  xmm4, xmmword ptr [rax + 4*rbx + 64]
        paddd   xmm4, xmm1
        paddd   xmm4, xmm2
        movdqu  xmm2, xmmword ptr [rax + 4*rbx + 80]
        paddd   xmm2, xmm3
        paddd   xmm2, xmm0
        movdqu  xmm0, xmmword ptr [rax + 4*rbx + 96]
        paddd   xmm0, xmm4
        movdqu  xmm1, xmmword ptr [rax + 4*rbx + 112]
        paddd   xmm1, xmm2
        add     rbx, 32
        add     rsi, 4
        jne     .LBB4_10
        test    rdi, rdi
        je      .LBB4_14
.LBB4_12:
        lea     rsi, [rax + 4*rbx]
        add     rsi, 16
        neg     rdi
.LBB4_13:                               # =>This Inner Loop Header: Depth=1
        movdqu  xmm2, xmmword ptr [rsi - 16]
        paddd   xmm0, xmm2
        movdqu  xmm2, xmmword ptr [rsi]
        paddd   xmm1, xmm2
        add     rsi, 32
        inc     rdi
        jne     .LBB4_13
.LBB4_14:
        paddd   xmm0, xmm1
        pshufd  xmm1, xmm0, 78          # xmm1 = xmm0[2,3,0,1]
        paddd   xmm1, xmm0
        pshufd  xmm0, xmm1, 229         # xmm0 = xmm1[1,1,2,3]
        paddd   xmm0, xmm1
        movd    ebx, xmm0
        cmp     r8, rdx
        je      .LBB4_5
        lea     rax, [rax + 4*rdx]
.LBB4_4:                                # =>This Inner Loop Header: Depth=1
        add     ebx, dword ptr [rax]
        add     rdx, 1
        add     rax, 4
        cmp     rcx, rdx
        ja      .LBB4_4
```

### ASM

First we have what is called the function prologue. We have the function label that mark its start. 

https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions

```asm
void time_data_directly:
        push    r14
        push    rbx
        push    rax
        mov     rax, qword ptr [rdi]
        mov     rcx, qword ptr [rdi + 8]
        sub     rcx, rax
```

In very basic terms, on ASM when you call a function there is a convention (actually there are various) on how you pass the parameters and how you get the return value back. We are using a x64 architecture so we need to understand its conventions.

```asm
    push r14
```

This first instruction exist because by the x64 convention, if our method uses the register r14, we, the function, the callee, must preserve its value, in other words, we must guarantee that at the end of our function, the register r14 has the same value as in the beginning. So that is why we push it to the stack. At the end we will pop this value back to the register r14.

You can see the contract about registers at:
https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions?redirectedfrom=MSDN&view=vs-2019#register-volatility-and-preservation

```
Register	Status	        Use
...
R12:R15	    Nonvolatile	    Must be preserved by callee
...
```

This is explained in the official documentation as:

```
This subsection discusses usage of each register. Registers %rbp, %rbx and %r12 through %r15 “belong” to the calling function and the called function is required to preserve their values. In other words, a called function must preserve these registers’ values for its caller. Remaining registers “belong” to the called function.
If a calling function wants to preserve such a register value across a
function call, it must save the value in its local stack frame.
```
http://web.archive.org/web/20160315222154/http://www.x86-64.org/documentation_folder/abi-0.99.pdf

In the same sense the next two lines are:

```asm
        push    rbx
        push    rax
```

By the guide we see that given that we are using the RBX register we need to save it and restore it. RBX is a very special register so it is good to have extra care with it.

```
Register	Status	        Use
...
RAX	Volatile	Return value register
RBX	Nonvolatile	Must be preserved by callee
...
```

The reason to push RAX here is kind of exoteric. In theory, RAX is how we return value from our function. So we will overwrite its value, so its makes no sense to save it here to restore it later. That is why the table says that RAX is volatile.

The problem is that x64 convention dictates that stackframes are 16 bytes aligned.

If we go back to the documentation we will see the following diagram:

```
Figure 3.3: Stack Frame with Base Pointer
-----------------------------------------------------------
Position    |           Contents            | Frame
--------------------------------------------|--------------
8n+16(%rbp) | memory argument eightbyte n   |
            | . . .                         | Previous
16(%rbp)    | memory argument eightbyte 0   |
--------------------------------------------|--------------
8(%rbp)     | return address                |
            |-------------------------------|
0(%rbp)     | previous %rbp value           |
            |-------------------------------|
-8(%rbp)    | unspecified                   | Current
            | . . .                         |
0(%rsp)     | variable size                 |
            |-------------------------------|
-128(%rsp)  | red zone                      |
            |-------------------------------|--------------
```

The previous frame contains the arguments that did not "fit" in the available registers. If you function has 10 arguments, for example, some will be passed using the stack. These are the "memory argument eightbyte" in the diagram. Obviously they are pushed to the stack by the "caller".

The return address is the exact instruction that will be called after our function is finished. But the "caller" pass this value to us using the RBP register. We push this value to the stack. That is why it is inside the "Current" frame. Important to notice that RBP and RBX are different register.

We are compiling in -O3, maximum performance and if we see at the compiler documentation (we are not using gcc but it serves our purpose here)

```
-fomit-frame-pointer
Don't keep the frame pointer in a register for functions that don't need one. This avoids the instructions to save, set up and restore frame pointers; it also makes an extra register available in many functions. It also makes debugging impossible on some machines.

Enabled at levels -O, -O2, -O3, -Os.
```
https://gcc.gnu.org/onlinedocs/gcc-3.4.6/gcc/Optimize-Options.html

That is why we are seeing this prologue. If we disable optimizations we would see a more "default" prologue, like (actually output)

```
void time_data_directly
        push    rbp
        mov     rbp, rsp
        sub     rsp, 48
```

This actually help us to understand that we are using 48 bytes of local variables (RBP - RSP). But back to our code:


```asm
void time_data_directly:
        push    r14
        push    rbx
        push    rax                      <- We are here
        mov     rax, qword ptr [rdi]
        mov     rcx, qword ptr [rdi + 8]
        sub     rcx, rax
```

Now we can understand the new two instructions.

```asm
        mov     rax, qword ptr [rdi]
        mov     rcx, qword ptr [rdi + 8]
```

Now comes another detail. Windows and Linux have different conventions. One of the reasons you cannot call a library compile into one onto the another. The first parameters in the Windows convention is the RCX, on Linux it is the RDI. That is why we are seeing it here. 

We are using the first parameter as an address to memory, it means that it is a pointer (or a ref). And if we check in out code that is correct. It is indeed a reference.

Se were bringing to RAX the 64-bit word starting at the zero-th byte of the std::vector and the 64-bit word starting at the nine-th byte. What they are is impossible to know at this moment and it, off course, depends on the specific implementation.

Our next two instructions are

```asm
        sub     rcx, rax # https://www.felixcloutier.com/x86/sub
        je      .LBB4_1 # https://www.felixcloutier.com/x86/jcc
```

The SUB instruction is just a simple subtraction, but it is important to understand that the instruction also set some flags. In this case the one relevant to us is:

```
RCX -= RAX;
ZF = (RCX == 0);
```

Because JE here means "jump if ZF = TRUE". The logic behind the name is: "JE" actually stands for "jump if equal". Two numbers are equal if "a - b" equals zero.

To better understand what is happening here let us see what ".LBB4_1" means.

```asm
.LBB4_1:
        xor     ebx, ebx
        jmp     .LBB4_5
```

XOR used like this, with both operands as the same register is the same as zeroing that register. So we have "ebx = 0". And we "jump" to .LBB4_5. For now suffice to say the ".LBB4_5" is the std::cout part. So "RBX" (or EBX) here is the "accum" variable.

So now I think it is pretty clear what the compiler did. Let us rebuild here.

```c++
uint8_t* items_bytes = (uint8_t*)(void*)&items;
if(items_bytes[0] == items_bytes[8]) {
    ebx = 0;
    print(ebx);
}
```

Well... So if the 64-bit word starting at the zero-th byte is equal to the 64-bit word starting at the nine-th byte. Print 0.

I am very inclined to say the it is testing if the items is empty and it is printing zero directly. We can inpect the vector layout using clang wit hthe command.

```
> clang -cc1 -v -fdump-record-layouts vector.cpp
```

Sometimes when you run this command you get an error like below. This is a well known problem and it is is the clang FAQ (see at http://clang.llvm.org/docs/FAQ.html)

```
vector.cpp:1:10: fatal error: 'vector' file not found
#include <vector>
```

The best solution is to run

```
> clang -### -c main.cpp
clang version 8.0.1-svn363027-1~exp1~20190611212422.76 (branches/release_80)
Target: x86_64-pc-linux-gnu
Thread model: posix
InstalledDir: /usr/bin
 "/usr/lib/llvm-8/bin/clang" ... <- copy this line
```

Get the last line that is the complete command line. Remove the "quotes" paste back to your command line and add the "-fdump-record-layouts" flag. Not ideal, but it works.

```
*** Dumping AST Record Layout
         0 | class std::vector<int, class std::allocator<int> >
         0 |   struct std::_Vector_base<int, class std::allocator<int> > (base)
         0 |     struct std::_Vector_base<int, class std::allocator<int> >::_Vector_impl _M_impl
         0 |       class std::allocator<int> (base) (empty)
         0 |         class __gnu_cxx::new_allocator<int> (base) (empty)
         0 |       std::_Vector_base<int, class std::allocator<int> >::pointer _M_start
         8 |       std::_Vector_base<int, class std::allocator<int> >::pointer _M_finish
        16 |       std::_Vector_base<int, class std::allocator<int> >::pointer _M_end_of_storage
           | [sizeof=24, dsize=24, align=8,
           |  nvsize=24, nvalign=8]
```

And the good thing is that we were right ont he money. The first byte is the pointer "start" and after him is the "finish". (An addendum is that the vector size is calculated. Something to think about it.)

So the assembly code is indeed testing the vector is empty.