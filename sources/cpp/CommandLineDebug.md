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

Running in Windows and Linux generate diferent results.

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

To debug in Windows is very easy, one only need the cdb debugger.

    >cdb main.exe
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

"kn" is  variant of "k". It shows the call stack but with numbers on the left. These numbers will help us now because in this case we want to jump to "frame" 3. Luckly we can achieve this just typing ".frame 3".

    .frame (Set Local Context)
    https://docs.microsoft.com/en-us/windows-hardware/drivers/debugger/-frame--set-local-context-

    0:000> .frame 3
    03 00000077`d9b1fcd0 00007ff7`95fe6138 main!main+0x56

