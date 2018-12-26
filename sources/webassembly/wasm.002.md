# Running Webassembly inside browsers

In the [first part](./wasm.001.md) we saw how to compile and run WASM files outside the browser. Our intention here is to complete the build and run part of the workflow, but now running inside browsers.

## Creating host application

To facilitate the creation of a "hello world" example we will use [ParcelJS](http://www.parceljs.org). First, we need to install everything that is needed.

    > choco install nodejs -y
    > npm install -g parcel-bundler

Now we can create our host application

    > npm init -y
    > ni index.html
    > parcel index.html

You will probably see a error, but it is fine. It is because the html is empty.

    > parcel index.html
    Server running at http://localhost:1234
    ×  .\index.html: Cannot read property 'walk' of null
        at HTMLAsset.generate (HTMLAsset.js:207:14)
        at HTMLAsset.process (Asset.js:208:35)

You can now open the url at http://localhost:1234 and you will see something like:

![alt text][img-parcel-001]

    index.html
    <html>
        <head>
        </head>
        <body>
            Hello World!
        </body>
    </html>

    console
    > parcel index.html
    Server running at http://localhost:1234
    √  Built in 151ms.

OK! Now refresh the browser and we are ready to load the WebAssembly. 

First we will do the "ParcelJS"-way because it is easier. Then we will do everything from scratch.

## Loading WASM using ParcelJS integration

First step is to create the JS that will setup and call the WASM.

    index.html
    <html>
        <head>
            <script src=".\main.js"></script>
        </head>
        <body>
            Hello World!
        </body>
    </html>

As soon as you save this file you will see in the console.

    >  parcel .\index.html
    Server running at http://localhost:1234
    ×  Cannot read property 'html' of null
        at Bundler.createBundleTree (\Bundler.js:650:39)
        at Bundler.createBundleTree (\Bundler.js:692:12)
        at Bundler.bundle (\Bundler.js:284:14)

This happens because ParcelJS is watching modifications to our ./index.html and has already detected a dependency. We just need to create the main.js file.

    main.js
    console.log(1);

After you typed this simple line and save the JS file, ParcelJS will reload this JS file for you.

![alt text][img-parcel-002]

Now comes the ParcelJS-magic. All you need to do to call our WASM is:

    main.js
    import { main } from './.bin/main.001.wasm';
    console.log(main());

And that is all you really need to do. Remember that we are only exporting a "main" function that does nothing and return 0. Not very exciting, for sure, but it is already working.

    main.001.c
    EXPORT
    int main()
    {
        return 0;
    }

Although it works, this integration is very limited, because you can not fulfill the WASM imports.

## Loading WASM manually

The first thing we need to do is to disable this WASM integration. We can easily do this using: [parcel-plugin-disable-loaders](https://github.com/Kagami/parcel-plugin-disable-loaders). Our second task is to install [babel-polyfill](https://github.com/babel/babel/tree/master/packages/babel-polyfill) to allow us to use [async functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function).

    package.json
    {
        "dependencies": {
            "babel-polyfill": "^6.26.0",
            "parcel-bundler": "^1.11.0",
            "parcel-plugin-disable-loaders": "^1.0.3"
        },
        "parcelDisableLoaders": ["wasm"]
    }

After this. Stop and start parcel again because it does not watch config changes for its plugins. Now our main.js becomes:

    main.js
    import 'babel-polyfill';
    import mainUrl from './.bin/main.001.wasm';

    async function importWASM (fileUrl, imp) {
        const wasm = fetch(fileUrl);
        const { module, instance } = 
            await WebAssembly.instantiateStreaming(wasm, imp);
        return instance.exports;
    };

    async function run () {
        const { main } = await importWASM(mainUrl);
        console.log(main());
    };

    run();

Another interesting ParcelJS feature is that, when you import a file and ParcelJS does not process that file, it just gives you a URL to download the file. Here, we are just passing this URL to our method "importWASM". This method is very simple. It just fetches the file, compiles the WASM, instantiates a new module, and returns all exported functions back to you.

Ideally we would use the "module" variable. We could cache it, saving it in a IndexDB, for example. But up to this day, Chrome, for example does not support this yet.

So thats it. Very easy.

## Enabling Console.Log

Although you can debug a WASM, if you do not configure source maps correctly you will find yourself debugging assembly code.

![alt text][img-WASM-001]

To easy this burden and to start understanding how the integration between WASM and the browser works, we will enable the WASM module to call the "console.log" function.

The first thing here is to update the C/C++ application to call our log function.

    main.002.cpp
    #define EXPORT __attribute__((visibility("default")))
    namespace browser::console
    {
        extern void log(int i) asm("consoleLog");
    }

    EXPORT int add(int l, int r) asm("add");
    int add(int l, int r)
    {
        browser::console::log(1);
        return l + r;
    }

The first difference is that now, we are exporting the function add. This functions will, unsurprisingly, return the addition of two numbers, but, first, will call "console.log".

For this to work we must define a function as "extern". We will see what this generates inside the WASM file. Later will be more clear why we we are using the "asm("consoleLog")" and why we are first defining and later declaring the add function.

Now we can easily compile and link our new wasm. Given that we are using [nested namespaces](https://en.cppreference.com/w/cpp/language/namespace) we also need the flag to enable C++17.

    > clang --target=wasm32-unknown-unknown-wasm ./main.002.cpp -c -o .\.bin\main.002.o -O3 -std=c++17
    > wasm-ld -export .\.bin\main.002.o -o .\.bin\main.002.wasm --no-entry --allow-undefined

Pay special attention, also to the new flag "allow-undefined" of the wams-ld linker. Without it wasm-ld will fail because it does not have access to the body of browser::console::log. For example:

    > wasm-ld -export .\.bin\main.002.o -o .\.bin\main.002.wasm --no-entry
    wasm-ld.exe: error: .\.bin\main.002.o: undefined symbol: consoleLog

The final step is to wire the import together. For this, we do not need to do anything different when compiling and instantiating the WASM. We just need to pass our import object and everything will works. The "env" is needed because line 03 below.

    main.js
    ...
    async function run () {
        const { add } = await importWASM(mainUrl, {
            env: {
                consoleLog: x => console.log(x)
            }
        });
        console.log(add(1,2));
    };
    run();

## Peeking inside the WASM file

We can also peek how the WASM works under the hood with the tool [wasm2wat](https://github.com/WebAssembly/wabt).

    > wasm2wat.exe .\.bin\main.002.wasm
    00 (module
    01     (type (;0;) (func (param i32 i32) (result i32)))
    02     (type (;1;) (func (param i32)))
    03     (import "env" "consoleLog" (func $consoleLog (type 1)))
    04     (func $add (type 0) (param i32 i32) (result i32)
    05         i32.const 1
    06         call $consoleLog
    07         get_local 1
    08         get_local 0
    09         i32.add)
    10     (table (;0;) 1 1 anyfunc)
    11     (memory (;0;) 2)
    12     (global (;0;) (mut i32) (i32.const 66560))
    13     (export "memory" (memory 0))
    14     (export "add" (func $add)))

To understand how the wiring between WASM and browser works, the important line is line 03. In this line we define a new function that needs to be imported and is typed as "type 1", which is defined in line 02 as a func that has one parameter of type i32, a normal integer of 32 bits.

If we jump to and analyze the body of function $add on line 04 we will very easy understand how WebAssembly works. On this line, 04, we see that we define a function (func), named $add, it is of type 0 (defined on line 1), receives two parameters of type i32 and return another i32.

Its first instruction (line 05) pushes onto the global stack the value 1 and calls the function $consoleLog (line 06). That is how you call functions on WASM. You put them on the stack and pop the result.

After this we read our parameters (line 07, 08) and call the instruction i32.add (line 09), that will sum the first two values of the global stack and put the result back on the stack. Given that our return value is the sum we do not need to do anything to return it.

Our last step is to export the $add function, to allow it to be called. We do this on line 14.

Now we are ready to answer why we need that "asm(...)" thing on our C++ code. The answer is very simple and it exists for historic reasons. If we remove them, we will have the following WASM code:

    00 (module
    01   (type (;0;) (func (param i32 i32) (result i32)))
    02   (type (;1;) (func (param i32)))
    03   (import "env" "_ZN7browser7console3logEi" (func $_ZN7browser7console3logEi (type 1)))
    04   (func $_Z3addii (type 0) (param i32 i32) (result i32)
    05     i32.const 1
    06     call $_ZN7browser7console3logEi
    07     get_local 1
    08     get_local 0
    09     i32.add)
    10   (table (;0;) 1 1 anyfunc)
    11   (memory (;0;) 2)
    12   (global (;0;) (mut i32) (i32.const 66560))
    13   (export "memory" (memory 0))
    14   (export "_Z3addii" (func $_Z3addii)))

"_ZN7browser7console3logEi" and "_Z3addii" are how the C++ compiler generates the name of our functions. This is called [name mangling](https://en.wikipedia.org/wiki/Name_mangling) and exists since the old-asm days. It is a fine thing to know since those days are coming back! Without the "asm(..)) you will need to correctly use these strange names when wiring the imports/exports.

[img-parcel-001]: https://gitcdn.link/repo/xunilrj/sandbox/master/sources/webassembly/parcel001.PNG "ParcelJS error because of empty file"
[img-parcel-002]: https://gitcdn.link/repo/xunilrj/sandbox/master/sources/webassembly/parceljs-001.gif "ParcelJS hot reload"
[img-WASM-001]: https://gitcdn.link/repo/xunilrj/sandbox/master/sources/webassembly/wasm001.PNG "WASM opcodes"