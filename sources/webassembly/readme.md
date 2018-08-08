# WebAssembly with C/C++

Good sense dictates that one introduces a subject as gentle as possible for you audience. That is not what we are going to do here.

All possible tutorials for webassembly somehow start using scripten and porting normal c/c++ applications to webassembly. I very sage approach indeed, if you have already what to port. But here we will str from scratch. How much from scratch you ask? You will see from the next step.

## Compiling LLVM

Yes, you will compile your own c/c++ compiler. Later will become clearer every step needed to compile your c/c++ code to webassembly. But for now we need to prepare our tools.

There are various possible paths to compile LLVM and clang but here we will use the simplest possible for a Windows scenario.

You will need:

### Install Chocolatey

    iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

### Install CMake

    choco install CMake -y

### Install Visual Studio 

    choco install visualstudio2017community -y

### Compile LLVM

    cd c:/
    mkdir ./llvm
    cd ./llvm

    function IfNot($Path){
        $r = Test-Path $Path
        if($r -eq $false) { $Path }
    }
    pushd

    IfNot "./llvm-6.0.1.src.tar.xz" | % {
        wget http://releases.llvm.org/6.0.1/llvm-6.0.1.src.tar.xz -OutFile $_
    }
    IfNot "./llvm-6.0.1.src.tar" | % {
        7z x llvm-6.0.1.src.tar.xz -y
    }
    IfNot "./llvm" | % {
        7z x llvm-6.0.1.src.tar -y
    }
    IfNot "llvm" | %{
        Rename-Item ./llvm-6.0.1.src "llvm"
    }

    cd ./llvm/tools

    IfNot "./cfe-6.0.1.src.tar.xz" | % { 
        wget http://releases.llvm.org/6.0.1/cfe-6.0.1.src.tar.xz -OutFile $_
    }
    IfNot "./cfe-6.0.1.src.tar" | % {
        7z x cfe-6.0.1.src.tar.xz -y
    }
    IfNot "./clang" | % {
        7z x cfe-6.0.1.src.tar -y
    }
    IfNot "./clang" | %{
        Rename-Item cfe-6.0.1.src "clang"
    }

    cd ..
    mkdir ./build -Force
    mkdir ./bin -Force
    cd build

    $binpath = Resolve-Path ..\bin
    cmake -G "Visual Studio 15 2017 Win64" -DCMAKE_BUILD_TYPE="Release" -DCMAKE_INSTALL_PREFIX=$binpath -DLLVM_TARGETS_TO_BUILD= -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly ..
    cmake --build . --target ALL_BUILD

    popd
    trap{
        popd
    }

The build will take a VERY LONG TIME.

## Compiling

After that we can start to understand all the mechanics. We can start with a very simple application. No. It is not going be the classic hello world. You will understand why.

    //main.c
    int main()
    {
        return 0;
    }

    > clang -emit-llvm --target=wasm32 -S ./main.001.c -o .bin/main.001.ll 

Important flags here are:

- -emit-llvm this will not generate an executable file. We will generate the LLVM intermediate language (IL).
- -target=wasm32 this will tell clang to actually  generate webassembly.

Let us dive into the .ll file generate.

    ; ModuleID = './main.001.c'
    source_filename = "./main.001.c"
    target datalayout = "e-m:e-p:32:32-i64:64-n32:64-S128"
    target triple = "wasm32"

The most interesting part here is the triple. That states that we want "WASM32", or, WebAssembly. If you do not set the wasm32 flag it will generate based on your machine. For example:

    target triple = "x86_64-pc-windows-msvc19.14.26433"

After that we have our main function:

    ; Function Attrs: noinline nounwind optnone
    define hidden i32 @main() #0 {
    entry:
        %retval = alloca i32, align 4
        store i32 0, i32* %retval, align 4
        ret i32 0
    }

One can see that we have a hidden function called @main that returns i32. This functions allocate on the stack an i32 variable with name %retval. Store the value zero at this variable. And return zero.

The "#0" after the function is a identifier for a lot of attributes for the function:

    attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }

After that we just have a set of LLVM metadata for this module.

    !llvm.module.flags = !{!0}
    !llvm.ident = !{!1}
    !0 = !{i32 1, !"wchar_size", i32 4}
    !1 = !{!"clang version 6.0.1 (tags/RELEASE_601/final)"}

## Linking

Our next step is to link this LLVM IL file to the webasm format.

    > llc -march=wasm32 -filetype=asm ./.bin/main.001.ll -o ./.bin/main.001.s

With this we have the WebAssembly assembly. And it is very similar to other Stack based virtual machines such as .NET or JAVA. Let us see.


	.text
	.file	"main.001.c"
	.hidden	main                    # -- Begin function main
	.globl	main
	.type	main,@function

First we have out .text section and some metadata.

    main:                           # @main
	    .result 	i32
        # %bb.0:                     # %entry
	    i32.const	$push2=, 0
	    i32.load	$push1=, __stack_pointer($pop2)
	    i32.const	$push3=, 16
	    i32.sub 	$push4=, $pop1, $pop3
	    i32.const	$push0=, 0
	    i32.store	12($pop4), $pop0
	    i32.const	$push5=, 0
        # fallthrough-return: $pop5
	    .endfunc

First we define a label/position for out function using "main:". Then we define that our function return a i32. 

    .Lfunc_end0:
	    .size	main, .Lfunc_end0-main
        # -- End function
    	.ident	"clang version 6.0.1 (tags/RELEASE_601/final)"

This label helps to calculate the size of the main function in bytes.

# The Whole Process

Now that we understand what is behind the curtain. Let us generate a real WebAssembly.

    > clang --target=wasm32-unknown-unknown-wasm ./main.001.c -c -o .\.bin\main.001.o -O3
    > lld -flavor wasm -export .\.bin\main.001.o -o .\.bin\main.001.wasm --no-entry

    or 

    > clang --target=wasm32-unknown-unknown-wasm ./main.001.c -c -o .\.bin\main.001.o -O3
    > wasm-ld -export .\.bin\main.001.o -o .\.bin\main.001.wasm --no-entry

This will generate a binary version of the wasm. We can see the textual representation of the wasm file doing.

    > .\binaryen\build\bin\wasm-dis.exe .\.bin\main.001.wasm

    (module
        (type $0 (func (result i32)))
        (global $global$0 (mut i32) (i32.const 66560))
        (table 1 1 anyfunc)
        (memory $0 2)
        (export "memory" (memory $0))
        (func $main (; 0 ;) (type $0) (result i32)
            (i32.const 0)
        )
        ;; custom section "linking", size 3
    )

To easily test our dummy wasm I created a very simple Wasm engine running on c# (LINK). We can run like (you will need DotNet Core 2.1)

    > dotnet dotwasm.dll ./bin/main.001.wasm
    Loading: C:\github\sandbox\sources\webassembly\.bin\main.001.wasm
    Done.
    > main()
    0

And the engine return our zero. Hardly impressive per se. But now we have a compiled C++ code running inside a sandboxed engine in a .NET application.
