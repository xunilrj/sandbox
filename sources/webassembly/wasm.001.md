# WebAssembly with C/C++

[Series](./readme.md)

PS: You can use clang directly now. No need to compile LLVM anymore. But where is the fun in it?  

Good sense dictates that one introduces a subject as gentle as possible for ones audience. That is not what we are going to do here.

All possible tutorials for WebAssembly somehow start using [emscripten](https://github.com/kripken/emscripten) and porting normal C/C++ applications to WebAssembly. It is a very sage approach indeed, if you have already what to port. But here we will try to do everything from scratch. How much from the scratch you ask? You will see from the next step.

## Compiling the compiler (LLVM)

Yes, you will compile your own C/C++ compiler. Later will become clearer every step needed to compile your C/C++ code to WebAssembly. But for now we need to prepare our tools.

There are various possible paths to compile [LLVM](http://llvm.org/) and [clang](https://clang.llvm.org/) but here we will use the simplest possible for a Windows scenario.

You will need:

### Installing Dependencies for Windows

    iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

    choco install CMake -y
    choco install visualstudio2019community -y
    choco install python3 -y

### Installing Dependencies for Ubuntu

    sudo apt-get install build-essential
    sudo apt-get install software-properties-common
    sudo add-apt-repository ppa:george-edison55/cmake-3.x
    sudo apt-get update
    sudo apt-get install cmake

    sudo add-apt-repository ppa:deadsnakes/ppa
    sudo apt install python3.8

### Building

    git clone https://github.com/llvm/llvm-project.git --depth 1

    cd ..
    mkdir ./build -Force
    mkdir ./bin -Force
    cd build

    $binpath = Resolve-Path ..\bin
    cmake -A x64 -DCMAKE_BUILD_TYPE="Release" -Thost=x64 -DCMAKE_INSTALL_PREFIX=$binpath -DLLVM_TARGETS_TO_BUILD=all ../llvm 
    cmake --build . --target ALL_BUILD

    popd
    trap{
        popd
    }

The build will take a VERY LONG TIME.

## Compiling our WASM file

After that we can start to understand all the mechanics. We can start with a very simple application. No. It is not going be the classic hello world. You will understand why.

    //main.c
    int main(int argc, char **argv)
    {
        return 0;
    }

    > clang -emit-llvm --target=wasm32 -S main.001.c -o main.001.ll 

Important flags here are:

- -emit-llvm this will not generate an executable file. We will generate the LLVM intermediate language (IL).
- -target=wasm32 this will tell clang to actually generate Webassembly.

## Peeking inside the LLVM ll file

Let us dive into the .ll file generate.

    ; ModuleID = 'main.001.c'
    source_filename = "main.001.c"
    target datalayout = "e-m:e-p:32:32-i64:64-n32:64-S128"
    target triple = "wasm32"
  

The most interesting part here is the triple. That states that we want "WASM32", or, WebAssembly. If you do not set the wasm32 flag it will generate based on your machine. For example:

    target triple = "x86_64-pc-windows-msvc19.14.26433"

To understand "target datalayout" see: https://llvm.org/docs/LangRef.html#data-layout. But we can summarize here:

    e = little-endian.
    m:e = names are mangled using ELF mangling. Private symbols get a .L prefix.
    p:32:32 = pointer size: 32 bits.
    i64:64 = integer type alignment.  
    n32:64 = set of native integer widths.
    S128 = natural alignment of the stack in bits.

After that we have our main function:

    ; Function Attrs: noinline nounwind optnone
    define i32 @main(i32 %0, i8** %1) #0 {
    %3 = alloca i32, align 4
    %4 = alloca i32, align 4
    %5 = alloca i8**, align 4
    store i32 0, i32* %3, align 4
    store i32 %0, i32* %4, align 4
    store i8** %1, i8*** %5, align 4
    ret i32 0
    }}

One can see that we have a function called @main that returns i32. This functions returns zero.

The "#0" after the function is a identifier for a lot of attributes for the function:

    attributes #0 = { noinline nounwind optnone "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="none" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "unsafe-fp-math"="false" "use-soft-float"="false" }

After that we just have a set of LLVM metadata for this module.

    !llvm.module.flags = !{!0}
    !llvm.ident = !{!1}

    !0 = !{i32 1, !"wchar_size", i32 4}
    !1 = !{!"clang version 10.0.0-4ubuntu1 "}

## Linking the WASM file

Our next step is to link this LLVM IL file to the webasm format.

    > llc -march=wasm32 -filetype=asm main.001.ll -o main.001.s

With this we have the WebAssembly assembly. And it is very similar to other Stack based virtual machines such as .NET or JAVA. Let us see.

    main:                                   
	.functype	main (i32, i32) -> (i32)
	.local  	i32
        global.get	__stack_pointer
        i32.const	16
        i32.sub 
        local.tee	2
        i32.const	0
        i32.store	12
        local.get	2
        local.get	0
        i32.store	8
        local.get	2
        local.get	1
        i32.store	4
        i32.const	0
        end_function

## The Whole Process

Now that we understand what is behind the curtain. Let us generate a real WebAssembly.

    > clang --target=wasm32 main.001.c -c -o main.001.o -O3
    > wasm-ld main.001.o -o main.001.wasm --no-entry --export-dynamic

This will generate a binary version of the wasm. We can see the textual representation of the wasm file using [wabt/bin/wasm2wat](https://github.com/WebAssembly/wabt).

    > wabt/bin/wasm2wat main.001.wasm

    (module
        (type $t0 (func (param i32 i32) (result i32)))
        (func $main (type $t0) (param $p0 i32) (param $p1 i32) (result i32)
            i32.const 0)
        (table $T0 1 1 funcref)
        (memory $memory 2)
        (global $g0 (mut i32) (i32.const 66560))
        (export "memory" (memory 0))
        (export "main" (func $main)))

After optimizations, all the main function does is "put" 0 on the stack. In a stack based language this means "return 0".

To easily test our dummy wasm we can use [wabt/bin/wasm-interp](https://github.com/WebAssembly/wabt).

    > wabt/bin/wasm-interp main.001.wasm

And we see nothing. Hardly impressive per se. But now we know everything is working.

## Using wasmer

Another cool possibility is to use [wasmer](https://wasmer.io/).

    > wasmer run ./bin/main.001.wasm