# More complex interactions

In the first part of the series, we were compiled the LLVM and generated a simple WASM file. 

In the second part, we understood how we call WASM functions from Javascript; how Webassembly code can call Javascript functions; and how the Webassembly stack works.

We are now ready to continue advancing:

1 - We will create a simple function to sum vectors;
2 - We will return a C++ struct by value;
3 - We will dive more in-depth on the interaction between Javascript/Webassembly;

# Vector Math lib

Ou code today is straightforward.

    //main.003.cpp
    template <typename T>
    struct vec3
    {
        T x, y, z;

        vec3<T> operator + (const vec3<T>& r) const
        {
            return {x+r.x, y+r.y, z+r.z};
        }
    };

So to begin, let us first try to compile this code.

    > clang++-10 --target=wasm32 main.003.cpp -c -o main.003.o 
    > wasm-ld main.003.o -o main.003.wasm --no-entry --export-dynamic
    > wasm2wat main.003.wasm  
    (module
        (table (;0;) 1 1 funcref)
        (memory (;0;) 2)
        (global (;0;) (mut i32) (i32.const 66560))
        (export "memory" (memory 0)))

And... we see nothing.

Nothing is being exported, so there is no code in the Wasm.

So let us export something. A simple function that creates our ```vec3<float>```.

    using vec3f = vec3<float>;

    __attribute__((export_name("make_vec3f")))
    vec3f
    make_vec3f(float x, float y, float z) { return {x, y, z}; }

If we compile and link now, we get:

    > clang++-10 --target=wasm32 main.003.cpp -c -o main.003.o -O3
    > wasm-ld main.003.o -o main.003.wasm --no-entry --export-dynamic
    > wasm2wat main.003.wasm  

    (module
        (type (;0;) (func (param i32 f32 f32 f32)))
        (func $make_vec3f_float__float__float_ (type 0) (param i32 f32 f32 f32)
            local.get 0
            local.get 3
            f32.store offset=8
            local.get 0
            local.get 2
            f32.store offset=4
            local.get 0
            local.get 1
            f32.store)
        (table (;0;) 1 1 funcref)
        (memory (;0;) 2)
        (global (;0;) (mut i32) (i32.const 66560))
        (export "memory" (memory 0))
        (export "make_vec3f" (func $make_vec3f_float__float__float_)))

Important parts here are:

    (export "make_vec3f" (func $make_vec3f_float__float__float_))

We are exporting something called "make_vec3f", and this something is a "func" named "$make_vec3f_float__float__float_". 

If you are wondering what this _float__float__float_" shenanigan is all about, this is how C++ compiler allows you to overload a function. Each argument type is appended to the function name. That is why when doing "static dispatch" you need to choose the argument's type at compile time.

https://en.wikipedia.org/wiki/Static_dispatch

Looking at our function's body, we see:

    (func $make_vec3f_float__float__float_ (type 0) (param i32 f32 f32 f32)
        local.get 0
        local.get 3
        f32.store offset=8
        local.get 0
        local.get 2
        f32.store offset=4
        local.get 0
        local.get 1
        f32.store)

The first line is pretty self-explanatory. We have a "func" called "$make_vec3f_float__float__float_" that is typed as "type 0" and it accepts four parameters: one i32 and three f32. But wait!

From where this "i32" came from? We never declared an i32 parameter.

And if you peek at the "type 0" at the first line.

    (type (;0;) (func (param i32 f32 f32 f32)))

You will see that this function type does not return anything.

Our original function took three floats and returned a struct. Now we have a function that takes four arguments and returns nothing. What is happening here?

If we analyze the generated code, hopefully, we will understand. Let us start with the first three operations

    local.get 0
    local.get 3
    f32.store offset=8

Remember that Wam is stack based virtual machine:

    local.get i: copy the value of the local variable i and put it on the stack;
    f32.store offset=n: take two values from the stack, named as: addr and value; Set memory[addr+offset] = value;

So, we can translate these three operations to something like:

    memory[local0 + 8] = local3

If we do the samething for the rest of the operations, we get:

    memory[local0 + 8] = local3
    memory[local0 + 4] = local2
    memory[local0] = local1

Now it is pretty clear what LLVM is doing here. We need to pass as the first parameter a memory pointer, that is where the result will be stored.

Of course, the LLVM team decide to follow the good n' old approach of using a pointer as the "out" argument, ad used in C.

    void
    make_vec3f(vec3f *out, float x, float y, float z) { return *out = {x, y, z}; }


But this is not the only possible approach in Webassembly, especially because Wasm function CAN return multiple arguments.

In C++, the new way to return multiple arguments is to return std::tuple instead of accepting them as pointers.

    //old
    void sum_prod(int a, int b, int* sum, int* prod)
    {
        sum = a+b;
        prod = a*b;
    }

    //new
    std::tuple<int,int> sum_prod(int a, int b)
    {
        return {a+b,a*b};
    }

We can do the same thing directly in Wasm. We just need to adjust our function type to return three floats. Of course, that in this case, you would not have a vec3f, you would have three floats. But worth mention nonetheless.

    (module
    (type (;0;) (func (param f32 f32 f32) (result f32 f32 f32)))
    (func $make_vec3f_float__float__float_ (type 0) (param f32 f32 f32)
        local.get 0
        local.get 1
        local.get 2)
    (export "make_vec3f" (func $make_vec3f_float__float__float_)))

The problem is that although the specification let us return multiple values. Other parts, demands that function return just one. Today some browsers fail to instantiate this Webassembly module. Maybe in the future.

    > Uncaught (in promise) CompileError: WebAssembly.instantiateStreaming(): return count of 3 exceeds internal limit of 1 @+16

Also see: https://releases.llvm.org/8.0.0/docs/ReleaseNotes.html#changes-to-the-webassembly-target

    one anticipated use for it will be to add support 
    for returning small structs as multiple return values,
    once the underlying WebAssembly platform itself supports it. 

# Calling the generated Wasm

Ok, we need to pass a pointer as the first parameter. But how do we chose this value?

First, let us create our environment to do some tests.

This time we will use another tool, snowpack (https://www.snowpack.dev/#snowpack-dev):

    > npm install -g npx
    > npm i snowpack
    > npx snowpack dev
    http://localhost:8080 > http://172.25.199.189:8080
    Server started in 8ms.

Now we create a simple HTML file.

    <html>
    <body>
        <script>
            async function run() {
                const wasm = await WebAssembly.instantiateStreaming(fetch("main.003.wasm"));
                const { make_vec3f, memory } = wasm.instance.exports;
                console.log(make_vec3f(0, 1.0, 2.0, 3.0));
            }
            run();
        </script>
    </body>
    </html>

Ok! This code should give any C/C++ developer the creeps. The browser will try to write to address zero.

The problem here is... zero is a valid Wasm memory address!

A line like the one below is valid:

    console.log(memory[0]);

But to check the result of the function, we need to read from the memory. The best way to do this is:

    const dv = new DataView(memory.buffer, 0);
    console.log(dv.getFloat32(0, true));
    console.log(dv.getFloat32(4, true));
    console.log(dv.getFloat32(8, true));
    > 1
    > 2
    > 3

So "0" as the memory address worked, but is it safe? How do I know that I am not overwriting anything? Or that no other code is going to overwrite my data?

To answer this, we need to go deeper in the rabbit hole.

# Wasm Memory

If we peek the Wasm file again, we will find a mysterious line:

    (global $g0 (mut i32) (i32.const 66560))

It is clear that this line is declaring a global value. Ok. But why this value exists? We surely never declared it in our application. And what is 66560?

To understand from where this came from, let first do two things.

1 - Let us declare a global variable and two functions.

    static float data = 0.0f;
    __attribute__((export_name("set"))) void
    set(float a) { data = a; }
    __attribute__((export_name("get"))) float
    get() { return data; }

If we copile this code,  we get:

    (func $set_float_ (type $t1) (param $p0 f32)
        i32.const 0
        local.get $p0
        f32.store offset=1024)
    (func $get__ (type $t2) (result f32)
        i32.const 0
        f32.load offset=1024)
    (export "set" (func $set_float_))
    (export "get" (func $get__)))

We can see that "wasm-ld" chose to store "data" at memory[1024]. The question is, why?
I believe it is good to know that we can configure this location using:

    > wasm-ld main.003.o -o main.003.wasm --no-entry --export-dynamic --global-base=512

    (func $set_float_ (type $t1) (param $p0 f32)
        i32.const 0
        local.get $p0
        f32.store offset=512)
    (func $get__ (type $t2) (result f32)
        i32.const 0
        f32.load offset=512)

So, now, all global data start at "--global-base", in this case: memory[512].

One curious thing is that after changing our "--global-base" we also changed the "global $g0".

    (global $g0 (mut i32) (i32.const 66064))

This means that the old value: 66560 was actually 65536 + 1024, because the current value of 66064 is 65536 + 512 + 16. We know that 512 came from the "--global-base" flag. To understand what means 65536 and 16 here, let us try something:

    > wasm-ld main.003.o -o main.003.wasm --no-entry --export-dynamic --global-base=512 -z stack-size=1024 

We are asking "wasm-ld" to use just 1024 bytes for the stack. With this we have.

    (func $set_float_ (type $t1) (param $p0 f32)
        i32.const 0
        local.get $p0
        f32.store offset=512)
    (func $get__ (type $t2) (result f32)
        i32.const 0
        f32.load offset=512)
    (global $g0 (mut i32) (i32.const 1552))

Now out global variable is at the position 1552 or 512 + 1024 + 16. This means that our value "65536" above was the stack size.

We still have this mysterious 16 bytes. They are not so mysterious if you think that at memory[512] we have a 4-byte float number.

So the stack reserved memory could start at memory[516]. For some reason, "wasm-ld" chose to start at memory[512 + 16]. 

To put this "reason" under test, let us increase the size of our global variable. We know that we are using just 4 bytes. Let us increase to 8 bytes;

    static float data[] = {0.0f, 1.0f};
    __attribute__((export_name("set"))) void
    set(int i, float a) { data[i] = a; }
    __attribute__((export_name("get"))) float
    get(int i) { return data[i]; }

"wasm-ld" chose 16 bytes when we were using just 4, a 12 byte slack. We expect to see this slack to decrease to only 8 bytes. Or in other words, nothing else must change in our Wasm structure.

    (global $g0 (mut i32) (i32.const 1552))    
    (data $d0 (i32.const 512) "\00\00\00\00\00\00\80?"))

And this is exactly what we see here.

"$g0" is still at 1552. The difference now is that the default value of our array is set using the "data" command that is writing the binary equivalent of our floats at position 512. 

Let us tally how many bytes were written, and hope that it has 8 bytes—two 4-bytes float.

If you counted 7 bytes, it is because you forgot to count the "?". Tricky.

So we have two floats in little-endian:

1 - 00 00 00 00
2 - 00 00 80 3F

You can use a IEEE-754 Converter like https://www.h-schmidt.net/FloatConverter/IEEE754.html to see if we have 0.0f and 1.0f. And we have, indeed.

So let us create a bigger array to see if "wasm-ld" will choose to align the stack again. Now we are aiming for 20 bytes or 5 4-bytes floats.

    (global $g0 (mut i32) (i32.const 1568))
    (data $d0 (i32.const 512) "\00\00\00\00\00\00\80?\00\00\00@\00\00@@\00\00\80@"))

Ok. Let us see what we got.

Our stack is still 1024. Our base global is still 512. So 1568 - 1024 - 512 = 32. Bullseye! 

We are using 20 bytes, but "wasm-ld" is reserving 32 bytes for globals. Now we know the stack is 16 bytes aligned.

## Memory Layout

So now we know that our memory layout is:

    -----------------------------------------------------------------
    |             | Globals |       Stack      |       Heap         |
    |             |         | <- Grow this way | Grows this way ->  |
    -----------------------------------------------------------------

Read the above diagram carefully. We have a potentially huge problem.

The stack is growing downwards. What this means is that in the case of stackoverflow, you will write over your globals, most probably corrupting everything.

This does not sound good. You can even find this "warning" in the "wasm-ld" code at https://github.com/llvm/llvm-project/blob/d851fce4cb238d5fe85ce71002721dfc2330fa46/lld/wasm/Writer.cpp#L210.

Luckily, the code also give us the solution. "wasm-ld" has a flag named "--stack-first" that will invert this order. And in the case of stackoverflow, we will see an error. Much better.

So let us use it (Remember also to adjust the "--global-base")

 > wasm-ld main.003.o -o main.003.wasm --no-entry --export-dynamic --global-base=1024 -z stack-size=1024 --stack-first

    (memory $memory 1)
    (global $g0 (mut i32) (i32.const 1024))
    (export "memory" (memory 0))
    (data $d0 (i32.const 1024) "\00\00\00\00\00\00\80?\00\00\00@\00\00@@\00\00\80@"))

So, now, our "global data" starts at 1024. Nice.
But we completely lost track of the size of the globals. 

There is no easy or safe way to know where the "heap" could start.

Luckily "wasm-ld" has one more card up to its sleeve. 

We do have this value; the problem is that it is not being exported. Mainly because we are not asking for it, tell me that you do not like hidden options.

We can ask for this (and others, see --export-all) using the "--export" flag.

    > wasm-ld main.003.o -o main.003.wasm --no-entry --export-dynamic --global-base=1024 -z stack-size=1024 --stack-first --export=__heap_base

    (global $g0 (mut i32) (i32.const 1024))
    (global $__heap_base i32 (i32.const 1044))
    (export "__heap_base" (global 1))
    (data $d0 (i32.const 1024) "\00\00\00\00\00\00\80?\00\00\00@\00\00@@\00\00\80@"))

So now we got one more exported variable: "__heap_base" with value 1044. Not coincidentally our 1024 bytes of the stack plus 20 bytes of global data.

Our memory now is:

    --------------------------------------------------------
    |       Stack      |    Globals   |       Heap         |
    | <- Grow this way |              | Grows this way ->  |
    --------------------------------------------------------
                       ^              ^
                      $g0        __heap_base

And you know what!? This seems an excellent value to send to our function.

    <html>
    <body>
        <script>
            async function run() {
                const wasm = await WebAssembly.instantiateStreaming(fetch("main.003.wasm"));
                const { make_vec3f, memory, __heap_base } = wasm.instance.exports;
                console.log(make_vec3f(__heap_base, 1.0, 2.0, 3.0));

                const dv = new DataView(memory.buffer, __heap_base);
                console.log(dv.getFloat32(0, true));
                console.log(dv.getFloat32(4, true));
                console.log(dv.getFloat32(8, true));
            }
            run();
        </script>
    </body>
    </html>