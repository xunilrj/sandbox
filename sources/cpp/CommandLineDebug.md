# Debugging Native Code

Although feared, text mode/command line debugging is much easier than advertised. It is true, that IDE, specially Visual Studio really ease the process. But as with every sugar you are trading understanding for productivity. On a daily basis this is fine, off course, but it is no excuse to not understand what is happening.

So first we will create a buggy application and debug it. After we can dive deeper on how the debug actually works.

## Buggy code.

The good news here is that it is extremely easy to generate a buggy code in C++. The code below print something and try to access a null pointer. Let us compile it and run.

    main.cpp
    #include <iostream>
    #include <string>
    
    struct Person
    {
        std::string Name;
    };

    int main(int argc, char** argv)
    {
        std::cout << "Hello" << std::endl;

        Person* me = nullptr;
        me->Name = "Daniel";

        return 0;
    }

Compiling it:

    Windows
    clang++ main.cpp -o main.exe -std=c++1z

    Linux
    clang++ main.cpp -o main -std=c++1z

Running in Windows and Linux generate different results.

    Windows
    >./main.exe
    Hello before

    Linux
    > ./main
    Hello before
    Segmentation fault (core dumped)

The difference here is beyond this tutorial, but we see that neither application finished, as expected.

However we can debug the application to understand what happened.

## Debugging in Windows

To debug in Windows is very easy, one only need the cdb debugger. You can download the Windows SDK (https://developer.microsoft.com/en-us/windows/downloads/windows-10-sdk) or just the cdb.exe here (https://raw.githubusercontent.com/xunilrj/sandbox/master/sources/cpp/cdb.exe)

    > wget https://raw.githubusercontent.com/xunilrj/sandbox/master/sources/cpp/cdb.exe
    > Unblock-File cdb.exe
    > cdb main.exe
    Microsoft (R) Windows Debugger Version 10.0.15063.0 AMD64
    Copyright (c) Microsoft Corporation. All rights reserved.

    CommandLine: main.exe
    Symbol search path is: srv*
    Executable search path is:
    ModLoad: 00007ff7`33b50000 00007ff7`33b8f000   image00007ff7`33b50000
    ModLoad: 00007ffd`0a9a0000 00007ffd`0ab90000   ntdll.dll
    ModLoad: 00007ffd`08fa0000 00007ffd`09052000   C:\WINDOWS\System32\KERNEL32.DLL
    ModLoad: 00007ffd`07a40000 00007ffd`07ce3000   C:\WINDOWS\System32\KERNELBASE.dll
    (1a4.4d30): Break instruction exception - code 80000003 (first chance)
    ntdll!LdrpDoDebuggerBreak+0x30:
    00007ffd`0aa711dc cc              int     3
    0:000> 

The last line "0:000>" means that the debugger is expecting some command. All commands of all debuggers are very cryptic. They are very short mnemonics. They increase your productivity but they all are non-obvious.

The first thing we can do here is type "g<enter>". "g" here stands for "Go". Some IDEs call it "run", "play" or "start".  

    g (Go)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/g--go-

    0:000> g
    Hello before
    (1a4.4d30): Access violation - code c0000005 (first chance)
    First chance exceptions are reported before any exception handling.
    This exception may be expected and handled.
    *** WARNING: Unable to verify checksum for F:\github\sandbox\sources\cpp\timer\main.exe
    *** ERROR: Module load completed but symbols could not be loaded for F:\github\sandbox\sources\cpp\timer\main.exe
    main+0x1537:
    00007ff7`33b51537 483b4118        cmp     rax,qword ptr [rcx+18h] ds:00000000`00000018=????????????????
    0:000>

Ok. Now we see the "Hello before" message. And the "Access violation" error. cdb also warns me that this is the "first chance" handler. It allows me to do something even before the error is actually catch. We know that in this case this error is not catch, the error is propagated and kills the application.

To understand what is happening we can use the command "k" from "display stacK backtrace". It will print the current call stack.

    k, kb, kc, kd, kp, kP, kv (Display Stack Backtrace)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/k--kb--kc--kd--kp--kp--kv--display-stack-backtrace-

    0:000> k
    Child-SP          RetAddr           Call Site
    00000024`5d36fb60 00007ff7`33b514ef main+0x1537
    00000024`5d36fbd0 00007ff7`33b5149d main+0x14ef
    00000024`5d36fc10 00007ff7`33b51056 main+0x149d
    00000024`5d36fc50 00007ff7`33b55e98 main+0x1056
    00000024`5d36fcb0 00007ffd`08fb7bd4 main+0x5e98
    00000024`5d36fcf0 00007ffd`0aa0ce71 KERNEL32!BaseThreadInitThunk+0x14
    00000024`5d36fd20 00000000`00000000 ntdll!RtlUserThreadStart+0x21
    0:000>

Not very helpful. "main+0x1537" is our current method. "main" in this string came from the executable name, it has nothing to do with the main function. The problem here is that we are missing the debug information. In Windows parlance this is the "symbols". We can confirm that they are not loaded using the command "lm".

    lm (List Loaded Modules)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/lm--list-loaded-modules-

    0:000> lm
    start             end                 module name
    00007ff7`33b50000 00007ff7`33b8f000   main     C (no symbols)
    00007ffd`07a40000 00007ffd`07ce3000   KERNELBASE   (deferred)
    00007ffd`08fa0000 00007ffd`09052000   KERNEL32   (pdb symbols)          C:\Users\xunil\OneDrive\Apps\windbg\sym\kernel32.pdb\B6F0E183C96D92A697EFA1F4B153B8151\kernel32.pdb
    00007ffd`0a9a0000 00007ffd`0ab90000   ntdll      (pdb symbols)          C:\Users\xunil\OneDrive\Apps\windbg\sym\ntdll.pdb\CCF44CB12C5ED8C3916E51A14CC618421\ntdll.pdb
    0:000>

"main" and "no symbols" will tip you that we are missing symbols for this module. A module is any executable or .dll file. KKERNEL32 and NTDLL, for example, are standard Windows .dlls. If you use the Windows kernel you probably has these .dll loaded in your memory space.

Is very easy to load our symbols. You just need to type the command "ld" and the module name.

    ld (Load Symbols)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/ld--load-symbols-

    0:000> ld main
    Symbols already loaded for main
    0:000>

hm.... This is strange. The problem here is that we have not generated debug symbols. So "cdb" has already done everything it could. What we can do here is kill the debugger, recompile our application with "debug information" and start debugging again.

To kill the debug you type "q".

    q, qq (Quit)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/q--qq--quit-

    0:000> q
    quit:

And now we recompile the application with debug information using the "-g" flag.

    Windows
    clang++ -g main.cpp -o main.exe -std=c++1z

    Linux
    clang++ -g main.cpp -o main -std=c++1z

This will generate a main.pdb for Windows.

    > ls main*
    Directory: F:\github\sandbox\sources\cpp\timer
    Mode                LastWriteTime         Length Name
    ----                -------------         ------ ----    
    -a----       06/07/2019     17:34            347 main.cpp
    -a----       06/07/2019     19:35        1084928 main.exe    
    -a----       06/07/2019     19:35        9981952 main.pdb

and we start again

    >cdb main.exe
    Microsoft (R) Windows Debugger Version 10.0.15063.0 AMD64
    Copyright (c) Microsoft Corporation. All rights reserved.

    CommandLine: main.exe
    Symbol search path is: srv*
    Executable search path is:
    ModLoad: 00007ff7`95fd0000 00007ff7`960e0000   main.exe
    ModLoad: 00007ffd`0a9a0000 00007ffd`0ab90000   ntdll.dll
    ModLoad: 00007ffd`08fa0000 00007ffd`09052000   C:\WINDOWS\System32\KERNEL32.DLL
    ModLoad: 00007ffd`07a40000 00007ffd`07ce3000   C:\WINDOWS\System32\KERNELBASE.dll
    (2f3c.5230): Break instruction exception - code 80000003 (first chance)
    ntdll!LdrpDoDebuggerBreak+0x30:
    00007ffd`0aa711dc cc              int     3
    0:000> g
    Hello before
    (2f3c.5230): Access violation - code c0000005 (first chance)
    First chance exceptions are reported before any exception handling.
    This exception may be expected and handled.
    *** WARNING: Unable to verify checksum for main.exe
    main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x37:
    00007ff7`95fdb747 483b4118        cmp     rax,qword ptr [rcx+18h] ds:00000000`00000018=????????????????
    0:000> k
    Child-SP          RetAddr           Call Site
    00000077`d9b1fbe0 00007ff7`95fdb6ef main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x37
    00000077`d9b1fc50 00007ff7`95fdb69d main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x3f
    00000077`d9b1fc90 00007ff7`95fdb126 main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::operator=+0x1d
    00000077`d9b1fcd0 00007ff7`95fe6138 main!main+0x56
    (Inline Function) --------`-------- main!invoke_main+0x22
    00000077`d9b1fd30 00007ffd`08fb7bd4 main!__scrt_common_main_seh+0x10c
    00000077`d9b1fd70 00007ffd`0aa0ce71 KERNEL32!BaseThreadInitThunk+0x14
    00000077`d9b1fda0 00000000`00000000 ntdll!RtlUserThreadStart+0x21
    0:000> 

Much better! We can see that the error happens inside the "std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign" which is, of course, the method for string assignment. The import part is that now I can see that the error happened inside "main!main+0x56", or, my method "main" inside my module "main".

This is helpful, but does not nails down the problem. Would be much better to have more information. For example the line that the error occurred.

The good news is that this is possible. You can use the command "lsa ."

    ls, lsa (List Source Lines)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/ls--lsa--list-source-lines-

    0:000> lsa .
        2655: 
        2656:         basic_string& assign(_In_reads_(_Count) const _Elem * const _Ptr, _CRT_GUARDOVERFLOW const size_type _Count)
        2657:                 {       // assign [_Ptr, _Ptr + _Count)
        2658:                 auto& _My_data = this->_Get_data();
    >   2659:                 if (_Count <= _My_data._Myres)
        2660:                         {
        2661:                         _Elem * const _Old_ptr = _My_data._Myptr();
        2662:                         _My_data._Mysize = _Count;
        2663:                         _Traits::move(_Old_ptr, _Ptr, _Count);
        2664:                         _Traits::assign(_Old_ptr[_Count], _Elem());
    0:000>

It even beautifully points me the current line. The problem is that this is pointing to the stdlib code. I would rather have this pointing to my code. This is also possible and the easiest method is first to type "ln" and then jumping to the "frame" you want to debug. 

"Ok, but what is a frame?"

Just imagine that for every method you call you need memory space for yout local variables and to where you will return. This memory space is your "frame".

So let us try.

    k, kb, kc, kd, kp, kP, kv (Display Stack Backtrace)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/k--kb--kc--kd--kp--kp--kv--display-stack-backtrace-

    0:000> kn
    # Child-SP          RetAddr           Call Site
    00 00000077`d9b1fbe0 00007ff7`95fdb6ef main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x37   
    01 00000077`d9b1fc50 00007ff7`95fdb69d main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x3f   
    02 00000077`d9b1fc90 00007ff7`95fdb126 main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::operator=+0x1d
    03 00000077`d9b1fcd0 00007ff7`95fe6138 main!main+0x56
    04 (Inline Function) --------`-------- main!invoke_main+0x22
    05 00000077`d9b1fd30 00007ffd`08fb7bd4 main!__scrt_common_main_seh+0x10c
    06 00000077`d9b1fd70 00007ffd`0aa0ce71 KERNEL32!BaseThreadInitThunk+0x14
    07 00000077`d9b1fda0 00000000`00000000 ntdll!RtlUserThreadStart+0x21
    0:000>

"kn" is  variant of "k". It shows the call stack but with numbers on the left. These numbers will help us now because in this case we want to jump to "frame" 3. Luckily we can achieve this just typing ".frame /c 3".

    .frame (Set Local Context)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/-frame--set-local-context-

    0:000> .frame /c 3
    03 0000003c`8ab5fa20 00007ff6`dc756138 main!main+0x56
    rax=0000000000000006 rbx=00007ff6dc83ffbc rcx=0000000000000000
    rdx=0000000000000000 rsi=0000000000000000 rdi=000001f309664730
    rip=00007ff6dc74b126 rsp=0000003c8ab5fa20 rbp=0000000000000000
    r8=0000000000000006  r9=c6feff6b64686d60 r10=0000000000000011
    r11=8101010101010100 r12=0000000000000000 r13=0000000000000000
    r14=0000000000000000 r15=0000000000000000
    iopl=0         nv up ei pl nz na po nc
    cs=0033  ss=002b  ds=002b  es=002b  fs=0053  gs=002b             efl=00010206
    main!main+0x56:
    00007ff6`dc74b126 488d0d03360f00  lea     rcx,[main!std::cout (00007ff6`dc83e730)]
    0:000>

Now we list our source code again...

    0:000> lsa .
        12: 
        13:         Person* me = nullptr;
        14:         me->Name = "Daniel";
        15:
    >   16:         std::cout << "Hello after" << std::endl;
        17:         return 0;
        18:     }
    0:000>

And it is pointing to the... wrong line. hum... OK! There is a reason for this and it is explained in the next section.

### Digression 1 - Why the source line is "wrong"

To understand why this the case we need to understand what the "cdb" is doing. First, there is not concept for "where you are on previous stack frames". We do have the concept where we are now: it is called Instruction Pointer. It is one of the many registers the CPU has. We can see them with the "r" command.

    0:000> r
    rax=0000000000000006 rbx=00007ff657cfffbc rcx=0000000000000000
    rdx=0000000000000000 rsi=0000000000000000 rdi=000001a5a8cd4730
    rip=00007ff657c0b747 rsp=00000036366ffbf0 rbp=0000000000000000
    r8=0000000000000006  r9=c6feff6b64686d60 r10=0000000000000011
    r11=8101010101010100 r12=0000000000000000 r13=0000000000000000
    r14=0000000000000000 r15=0000000000000000
    iopl=0         nv up ei pl nz na po nc
    cs=0033  ss=002b  ds=002b  es=002b  fs=0053  gs=002b             efl=00010206
        2656:         basic_string& assign(_In_reads_(_Count) const _Elem * const _Ptr, _CRT_GUARDOVERFLOW const size_type _Count)
        2657:                 {       // assign [_Ptr, _Ptr + _Count)
        2658:                 auto& _My_data = this->_Get_data();
    >   2659:                 if (_Count <= _My_data._Myres)
        2660:                         {
        2661:                         _Elem * const _Old_ptr = _My_data._Myptr();
    main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x37:
    00007ff6`57c0b747 483b4118        cmp     rax,qword ptr [rcx+18h] ds:00000000`00000018=????????????????
    0:000> 

If we run "uf $ip" this will dump the assemby for each line of code for a function that wraps the memory address pointed b the uf parameter.

    0:000> uf $ip              
    main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2657]:
    2657 00007ff6`57c0b710 4883ec68        sub     rsp,68h
    2657 00007ff6`57c0b714 4c89442458      mov     qword ptr [rsp+58h],r8
    2657 00007ff6`57c0b719 4889542450      mov     qword ptr [rsp+50h],rdx
    2657 00007ff6`57c0b71e 48894c2448      mov     qword ptr [rsp+48h],rcx
    2657 00007ff6`57c0b723 488b4c2448      mov     rcx,qword ptr [rsp+48h]
    2658 00007ff6`57c0b728 4889ca          mov     rdx,rcx
    2658 00007ff6`57c0b72b 48894c2428      mov     qword ptr [rsp+28h],rcx
    2658 00007ff6`57c0b730 4889d1          mov     rcx,rdx
    2658 00007ff6`57c0b733 e8e865ffff      call    main!ILT+3355(?_Get_data?$_String_allocU?$_String_base_typesDV?$allocatorDstdstdstdQEAAAEAV?$_String_valU?$_Simple_typesDstd (00007ff6`57c01d20)
    2658 00007ff6`57c0b738 4889442440      mov     qword ptr [rsp+40h],rax
    2659 00007ff6`57c0b73d 488b442458      mov     rax,qword ptr [rsp+58h]
    2659 00007ff6`57c0b742 488b4c2440      mov     rcx,qword ptr [rsp+40h]
    2659 00007ff6`57c0b747 483b4118        cmp     rax,qword ptr [rcx+18h]
    2659 00007ff6`57c0b74b 0f875e000000    ja      main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x9f (00007ff6`57c0b7af)

    main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x41 [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2661]:
    2661 00007ff6`57c0b751 488b4c2440      mov     rcx,qword ptr [rsp+40h]
    2661 00007ff6`57c0b756 e83870ffff      call    main!ILT+6030(?_Myptr?$_String_valU?$_Simple_typesDstdstdQEAAPEADXZ) (00007ff6`57c02793)
    2661 00007ff6`57c0b75b 4889442438      mov     qword ptr [rsp+38h],rax
    2662 00007ff6`57c0b760 488b442458      mov     rax,qword ptr [rsp+58h]
    2662 00007ff6`57c0b765 488b4c2440      mov     rcx,qword ptr [rsp+40h]
    2662 00007ff6`57c0b76a 48894110        mov     qword ptr [rcx+10h],rax
    2663 00007ff6`57c0b76e 4c8b442458      mov     r8,qword ptr [rsp+58h]
    2663 00007ff6`57c0b773 488b542450      mov     rdx,qword ptr [rsp+50h]
    2663 00007ff6`57c0b778 488b4c2438      mov     rcx,qword ptr [rsp+38h]
    2663 00007ff6`57c0b77d e8dc59ffff      call    main!ILT+345(?move?$char_traitsDstdSAPEADQEADQEBD_KZ) (00007ff6`57c0115e)
    2664 00007ff6`57c0b782 c644243700      mov     byte ptr [rsp+37h],0
    2664 00007ff6`57c0b787 488b4c2438      mov     rcx,qword ptr [rsp+38h]
    2664 00007ff6`57c0b78c 48034c2458      add     rcx,qword ptr [rsp+58h]
    2664 00007ff6`57c0b791 488d542437      lea     rdx,[rsp+37h]
    2664 00007ff6`57c0b796 4889442420      mov     qword ptr [rsp+20h],rax
    2664 00007ff6`57c0b79b e8b567ffff      call    main!ILT+3920(?assign?$char_traitsDstdSAXAEADAEBDZ) (00007ff6`57c01f55)
    2664 00007ff6`57c0b7a0 488b442428      mov     rax,qword ptr [rsp+28h]
    2665 00007ff6`57c0b7a5 4889442460      mov     qword ptr [rsp+60h],rax
    2665 00007ff6`57c0b7aa e91e000000      jmp     main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0xbd (00007ff6`57c0b7cd)

    main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x9f [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2668]:
    2668 00007ff6`57c0b7af 4c8b4c2450      mov     r9,qword ptr [rsp+50h]
    2668 00007ff6`57c0b7b4 488b542458      mov     rdx,qword ptr [rsp+58h]
    2668 00007ff6`57c0b7b9 448a442430      mov     r8b,byte ptr [rsp+30h]
    2668 00007ff6`57c0b7be 488b4c2428      mov     rcx,qword ptr [rsp+28h]
    2668 00007ff6`57c0b7c3 e8c165ffff      call    main!ILT+3460(??$_Reallocate_forV<lambda_1>?0??assign?$basic_stringDU?$char_traitsDstdV?$allocatorD (00007ff6`57c01d89)   
    2668 00007ff6`57c0b7c8 4889442460      mov     qword ptr [rsp+60h],rax

    main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0xbd [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2672]:
    2672 00007ff6`57c0b7cd 488b442460      mov     rax,qword ptr [rsp+60h]
    2672 00007ff6`57c0b7d2 4883c468        add     rsp,68h
    2672 00007ff6`57c0b7d6 c3              ret
    0:000> 

Given that the RIP register has the value "00007ff657c0b747" we can see that we are at the line

    2659 00007ff6`57c0b747 483b4118        cmp     rax,qword ptr [rcx+18h]

The first column means line 2659 of the file "xstring", the "section" just above this line. And this is confirmed by the "k" command.

    0:000> k
    Child-SP          RetAddr           Call Site
    00000036`366ffbf0 00007ff6`57c0b6ef main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x37 [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2659]
    00000036`366ffc60 00007ff6`57c0b69d main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x3f [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2676]
    00000036`366ffca0 00007ff6`57c0b126 main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::operator=+0x1d [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2498]
    00000036`366ffce0 00007ff6`57c16138 main!main+0x56 [F:\github\sandbox\sources\cpp\timer\main.cpp @ 16]
    (Inline Function) --------`-------- main!invoke_main+0x22 [d:\agent\_work\3\s\src\vctools\crt\vcstartup\src\startup\exe_common.inl @ 78]
    00000036`366ffd40 00007ffd`08fb7bd4 main!__scrt_common_main_seh+0x10c [d:\agent\_work\3\s\src\vctools\crt\vcstartup\src\startup\exe_common.inl @ 288]
    00000036`366ffd80 00007ffd`0aa0ce71 KERNEL32!BaseThreadInitThunk+0x14
    00000036`366ffdb0 00000000`00000000 ntdll!RtlUserThreadStart+0x21
    0:000>

Now we can understand why the "k" command gives the "wrong" source line.  If we use a more detailed "k" command variant, "kp". This command will parse the stack and give us the information in the form of arguments and the address to where the function (actually the RIP register) will return.

    k, kb, kc, kd, kp, kP, kv (Display Stack Backtrace)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/k--kb--kc--kd--kp--kp--kv--display-stack-backtrace-

    0:000> kp
    Child-SP          RetAddr           Call Site
    0000008b`22baf880 00007ff6`57c0b6ef main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign(char * _Ptr = 0x00007ff6`57ccc1af "Daniel", unsigned int64 _Count = 6)+0x37 [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2659]
    0000008b`22baf8f0 00007ff6`57c0b69d main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign(char * _Ptr = 0x00007ff6`57ccc1af "Daniel")+0x3f [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2676]
    0000008b`22baf930 00007ff6`57c0b126 main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::operator=(char * _Ptr = 0x00007ff6`57ccc1af "Daniel")+0x1d 
    [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2498]
    0000008b`22baf970 00007ff6`57c16138 main!main(int argc = 0n1, char ** argv = 0x0000025e`ec4b4730)+0x56 [F:\github\sandbox\sources\cpp\timer\main.cpp @ 16]
    (Inline Function) --------`-------- main!invoke_main+0x22 [d:\agent\_work\3\s\src\vctools\crt\vcstartup\src\startup\exe_common.inl @ 78]
    0000008b`22baf9d0 00007ffd`08fb7bd4 main!__scrt_common_main_seh(void)+0x10c [d:\agent\_work\3\s\src\vctools\crt\vcstartup\src\startup\exe_common.inl @ 288]
    0000008b`22bafa10 00007ffd`0aa0ce71 KERNEL32!BaseThreadInitThunk+0x14
    0000008b`22bafa40 00000000`00000000 ntdll!RtlUserThreadStart+0x21
    0:000>

If we focus on the main line 

    0000008b`22baf970 00007ff6`57c16138 main!main(int argc = 0n1, char ** argv = 0x0000025e`ec4b4730)+0x56 [F:\github\sandbox\sources\cpp\timer\main.cpp @ 16]

the second column "00007ff6`57c16138" is where the main will return to when it is finished. So what we need to do is parse the line above this one. Because this line will have the addres to where, inside the main, that function will return to.

    0000008b`22baf930 00007ff6`57c0b126 main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::operator=(char * _Ptr = 0x00007ff6`57ccc1af "Daniel")+0x1d 

This function, the overloaded operator "=" will return to the address "00007ff6`57c0b126". We know, by our knowledge of the code, that this method will return to the "main" function. But we can confirm this with the "uf" command.

    main!main [F:\github\sandbox\sources\cpp\timer\main.cpp @ 10]:
    10 00007ff6`57c0b0d0 4883ec58        sub     rsp,58h
    10 00007ff6`57c0b0d4 c744245400000000 mov     dword ptr [rsp+54h],0
    10 00007ff6`57c0b0dc 4889542448      mov     qword ptr [rsp+48h],rdx
    10 00007ff6`57c0b0e1 894c2444        mov     dword ptr [rsp+44h],ecx
    11 00007ff6`57c0b0e5 488d0d44360f00  lea     rcx,[main!std::cout (00007ff6`57cfe730)]
    11 00007ff6`57c0b0ec 488d15ad100c00  lea     rdx,[main!`string' (00007ff6`57ccc1a0)]
    11 00007ff6`57c0b0f3 e85c79ffff      call    main!ILT+6735(??$?6U?$char_traitsDstdstdYAAEAV?$basic_ostreamDU?$char_traitsDstd (00007ff6`57c02a54)
    11 00007ff6`57c0b0f8 4889c1          mov     rcx,rax
    11 00007ff6`57c0b0fb 488d15ec75ffff  lea     rdx,[main!ILT+5865(??$endlDU?$char_traitsDstdstdYAAEAV?$basic_ostreamDU?$char_traitsDstd (00007ff6`57c026ee)]
    11 00007ff6`57c0b102 e87d8affff      call    main!ILT+11135(??6?$basic_ostreamDU?$char_traitsDstdstdQEAAAEAV01P6AAEAV01AEAV01ZZ) (00007ff6`57c03b84)
    13 00007ff6`57c0b107 48c744243800000000 mov   qword ptr [rsp+38h],0
    14 00007ff6`57c0b110 488b4c2438      mov     rcx,qword ptr [rsp+38h]
    14 00007ff6`57c0b115 488d1593100c00  lea     rdx,[main!`string' (00007ff6`57ccc1af)]
    14 00007ff6`57c0b11c 4889442430      mov     qword ptr [rsp+30h],rax
    14 00007ff6`57c0b121 e8866cffff      call    main!ILT+3495(??4?$basic_stringDU?$char_traitsDstdV?$allocatorD (00007ff6`57c01dac)
    16 00007ff6`57c0b126 488d0d03360f00  lea     rcx,[main!std::cout (00007ff6`57cfe730)]
    16 00007ff6`57c0b12d 488d1583100c00  lea     rdx,[main!`string' (00007ff6`57ccc1b7)]
    16 00007ff6`57c0b134 4889442428      mov     qword ptr [rsp+28h],rax
    16 00007ff6`57c0b139 e81679ffff      call    main!ILT+6735(??$?6U?$char_traitsDstdstdYAAEAV?$basic_ostreamDU?$char_traitsDstd (00007ff6`57c02a54)
    16 00007ff6`57c0b13e 4889c1          mov     rcx,rax
    16 00007ff6`57c0b141 488d15a675ffff  lea     rdx,[main!ILT+5865(??$endlDU?$char_traitsDstdstdYAAEAV?$basic_ostreamDU?$char_traitsDstd (00007ff6`57c026ee)]
    16 00007ff6`57c0b148 e8378affff      call    main!ILT+11135(??6?$basic_ostreamDU?$char_traitsDstdstdQEAAAEAV01P6AAEAV01AEAV01ZZ) (00007ff6`57c03b84)
    16 00007ff6`57c0b14d 4531c0          xor     r8d,r8d
    16 00007ff6`57c0b150 4889442420      mov     qword ptr [rsp+20h],rax
    17 00007ff6`57c0b155 4489c0          mov     eax,r8d
    17 00007ff6`57c0b158 4883c458        add     rsp,58h
    17 00007ff6`57c0b15c c3              ret

The address "00007ff6`57c0b126" in on the line 

    16 00007ff6`57c0b126 488d0d03360f00  lea     rcx,[main!std::cout (00007ff6`57cfe730)]

The first column is what interest at this moment, because it is the line of this instruction. So now we understand why the "k" command give us the "wrong source line". We can also antipate that, when the line contains more than one call, only the last one will be wrong. 

A compiler could fill the following line with a "nop" instruction just to correct this for debug mode, but release will always have this problem.

## Back to the exception analysis

We had just changed out "stack frame" context with 

    0:000> .frame /c 3
    ...
    0:000> lsa .
        12: 
        13:         Person* me = nullptr;
        14:         me->Name = "Daniel";
        15:
    >   16:         std::cout << "Hello after" << std::endl;
        17:         return 0;
        18:     }
    0:000>

We now know that the execution is actually on line 14. Although the "frame switch" command shows us the wrong source line it does give us good information to understand what is happening. For example, the "dv" command, "display local variables".

    dv (Display Local Variables)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/dv--display-local-variables-

    0:000> dv
        argc = 0n1
        argv = 0x0000025e`ec4b4730
          me = 0x00000000`00000000
    0:000> 

We can easily see that the application received one parameter, the list of parameters is at the "0x0000025e`ec4b4730" memory address, and what really matters here, the "me" is nullptr. That is our problem!

You can squeeze even more helpful information with

    0:000> dv /t /i
    prv param  int argc = 0n1
    prv param  char ** argv = 0x0000025e`ec4b4730
    prv local  struct Person * me = 0x00000000`00000000
    0:000>

## Improving usability

"OK. "cdb" does work, but wasn't all this a lot of work?" Yes, you are right! There is a lot to type just to start. But we can easily facilitate this with startup scripts. "cdb" automatically loads the file "ntsd.ini" when the debug starts. This is a huge security flaw so we are not going to use it. We are going to create a "main.cdb.ini" file that will configure everything we need.

    main.cdb.ini
    .lines -e
    l+s
    l+t
    lsp 6

This will basically run these commands when you start your session.

    >cdb -cf "main.cdb.ini" main.exe
    Microsoft (R) Windows Debugger Version 10.0.15063.0 AMD64
    Copyright (c) Microsoft Corporation. All rights reserved.

    CommandLine: main.exe
    Symbol search path is: srv*
    Executable search path is:
    ModLoad: 00007ff6`57c00000 00007ff6`57d10000   main.exe
    ModLoad: 00007ffd`0a9a0000 00007ffd`0ab90000   ntdll.dll
    ModLoad: 00007ffd`08fa0000 00007ffd`09052000   C:\WINDOWS\System32\KERNEL32.DLL
    ModLoad: 00007ffd`07a40000 00007ffd`07ce3000   C:\WINDOWS\System32\KERNELBASE.dll
    (231c.67d8): Break instruction exception - code 80000003 (first chance)
    ntdll!LdrpDoDebuggerBreak+0x30:
    00007ffd`0aa711dc cc              int     3
    0:000> .lines -e
    Line number information will be loaded
    0:000> l+s
    Source options are 4:
        4/s - List source code at prompt
    0:000> l+t
    Source options are 5:
        1/t - Step/trace by source line
        4/s - List source code at prompt
    0:000> lsp 6
    At the prompt, display 3 source lines before and 3 after
    0:000> 

".lines", "l+s", "l+t" amd "lsp" allow us to debug in "source mode". Were we see the source code.

    .lines (Toggle Source Line Support)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/-lines--toggle-source-line-support-

    l+, l- (Set Source Options)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/l---l---set-source-options-

    lsp (Set Number of Source Lines)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/lsp--set-number-of-source-lines-

After this, everything is setup. We can hit "g" and fall directly in the exception. One shortcut is to list all local variables of the call stack. We know hot to do this for each "stack frame". We just need to repeat it for every frame. There is a shortcut to do this. It is the command "!for_each_frame". 

    !for_each_frame

    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/-for-each-frame

    0:000> g
    ...
    0:000> !for_each_frame dv
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    00 000000dd`94effd10 00007ff6`57c0b6ef main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x37 [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2659]
        this = 0x00000000`00000000
        _Ptr = 0x00007ff6`57ccc1af "Daniel"
        _Count = 6
        _My_data = 0x00000000`00000000
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    01 000000dd`94effd80 00007ff6`57c0b69d main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x3f [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2676]
        this = 0x00000000`00000000
        _Ptr = 0x00007ff6`57ccc1af "Daniel"
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    02 000000dd`94effdc0 00007ff6`57c0b126 main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::operator=+0x1d [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2498]
        this = 0x00000000`00000000
        _Ptr = 0x00007ff6`57ccc1af "Daniel"
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    03 000000dd`94effe00 00007ff6`57c16138 main!main+0x56 [F:\github\sandbox\sources\cpp\timer\main.cpp @ 16]
        argc = 0n1
        argv = 0x000002ca`d6084730
        me = 0x00000000`00000000
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    04 (Inline Function) --------`-------- main!invoke_main+0x22 [d:\agent\_work\3\s\src\vctools\crt\vcstartup\src\startup\exe_common.inl @ 78]
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    05 000000dd`94effe60 00007ffd`08fb7bd4 main!__scrt_common_main_seh+0x10c [d:\agent\_work\3\s\src\vctools\crt\vcstartup\src\startup\exe_common.inl @ 288]
                            has_cctor = false
                            main_result = <value unavailable>
                    tls_init_callback = <value unavailable>
                            is_nested = <value unavailable>
                    tls_dtor_callback = <value unavailable>
                            main_result = <value unavailable>
    __scrt_current_native_startup_state = <value unavailable>
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    06 000000dd`94effea0 00007ffd`0aa0ce71 KERNEL32!BaseThreadInitThunk+0x14
    Unable to enumerate locals, Win32 error 0n87
    Private symbols (symbols.pri) are required for locals.
    Type ".hh dbgerr005" for details.
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    07 000000dd`94effed0 00000000`00000000 ntdll!RtlUserThreadStart+0x21
    Unable to enumerate locals, Win32 error 0n87
    Private symbols (symbols.pri) are required for locals.
    Type ".hh dbgerr005" for details.
    _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
    00 000000dd`94effd10 00007ff6`57c0b6ef main!std::basic_string<char,std::char_traits<char>,std::allocator<char> >::assign+0x37 [C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Tools\MSVC\14.16.27023\include\xstring @ 2659]
    0:000>

Although the result of this command is very verbose, we can see almost immediately where the problem is. "this = 0" is of course our biggest tip here that something is not right. If we follow the tip we will quickly arrive at "me = 0". The source lne is not 100% precise, as we saw above, but is good enough.