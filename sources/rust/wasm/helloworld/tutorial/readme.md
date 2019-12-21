# RUST and WebGL

## Setup

To install rust one can find everything needed at:

https://rustup.rs/

Or runnig this script:

    wget https://win.rustup.rs/x86_64 -Outfile rust-init.exe
    ./rust-init.exe -y
    rustup update
    cargo install wasm-pack
    mkdir html
    mkdir rust | pushd
        cargo new --lib -name rust_webgl
        wasm-pack build --out-dir ../html/wasm --target web
    popd

I think it is important to highlight that Rust like https://en.wikipedia.org/wiki/Snake_case  

### TOML file

To start we need to modify the TOM file to import the needed libraries.

    [package]
    name = "rust_webgl"
    version = "0.1.0"
    authors = ["Daniel Frederico Lins Leite <xunilrj@hotmail.com>"]
    edition = "2018"

    [lib]
    crate-type = ["cdylib"]

    [dependencies]
    wasm-bindgen = "0.2.54"
    js-sys = "0.3.31"

After this we can run

    wasm-pack build --out-dir ../html/wasm --target web


## Empty Page

The simples possible page here would just load the webassembly.

    <html>
        <body>        
            <script type="module">
                import init from './wasm/rust_webgl.js';
                async function start() {
                    await init();
                }
                start();
            </script>
        </body>
    </html>

We can serve this page directly with any webserver. We do not need any server processing. For example.

    npm install -g httpserver
    httpserver ./html


## Rust console

To make out demo a little more interesting we can print something to the browser console.

To do this we will use the library "wasm-bindgen" that will create the glue code for us.

    use wasm_bindgen::prelude::*;
    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace = console)]
        fn log(s: &str);
    }

    #[wasm_bindgen]
    pub fn greet(name: &str) {
        log(&format!("Hello, {}!", name));
    }

And in our HTML we do:

    async function start() {
        await init();
        greet("Daniel");
    }

And we have our "hello world" example done.

## Starting WebGL

To enable the Rust code generation to use WebGL directly from rust append your TOML file with

    [dependencies.web-sys]
    version = "0.3.4"
    features = [
    'Document',
    'Element',
    'HtmlCanvasElement',
    'WebGlBuffer',
    'WebGlRenderingContext',
    'WebGlProgram',
    'WebGlShader',
    'Window',
    ]

and build

    wasm-pack build --out-dir ../html/wasm --target web

## Rust and Error Treatment

Rust was creted as an safe language. This means that you cannot leak memory nor generate garbage using the copy/move/borrow semantics and that you must treat expected errors.

The first is similar to c++ value-copy semantic, move semantics and borrow is similar to pass by reference. The diference is that the Rust compiler expects you to follow these semantics stricly.

For expected errors Rust follow the Maybe and Either monads from function language. Clicke here if you want to understand them better. But here they are called Option/Result respectively. An summary if you never heard of them is:

### Maybe/Option

Can or cannot have a value. So you can return a value or no value at all from function.

    fn div(a: int32, b: int32) -> Option<int32> {
        if(b == 0) return None;
        else return Some(a/b);
    }

    let r = div(1,2);
    match r {
        None => printf!("Error"),
        Some(v) => printf("Result {}", v),
    }

This is the basic usage of Option.

### Either/Result

Result is very similar, but instead of having only one "error", now you can have any error you want. 

    fn div(a: int32, b: int32) -> Result<int32, DivError> {
        if(b == 0) return Err(DivError::DivByZero);
        else return Ok(a/b);
    }

    let r = div(1,2);
    match r {
        Err(err) => printf!("Error: {}", err),
        Ok(v) => printf("Result {}", v),
    }

One advantage of using Either/Result is that there is an language sugar on top of it. So if you need to chain multiples operations that can fail, you can use the operator "?".

    pub fn operation(a: int32, b: int32) -> Result<int32, DivError> {
        let r1 = div(a, b)?;
        let r2 = div(r1, 3);
        return r2;
    }

You can understand the "?" here as:

    pub fn operation(a: int32, b: int32) -> Result<int32, DivError> {
        let r1 = match div(a, b) {
            Err(err) => return From::from(err),
            Ok(v) => v,
        }
        let r2 = div(r1, 3);
        return r2;
    }

Important points:  
- "r1" is not Result<> anymore. Now it is a full realized int32.
- if "r1" was an error, it propagates the error automacally for you.
- It converts that operation error to your function error. If they are not the same.

## Converting between Option <-> Result

We now see the real gain of using Result and the "?" operator. But it only works for Result<> not Option<> and a lot of APIs return Option<>. One simple option is to use the .ok_or() method. 

    fn div(a: int32, b: int32) -> Option<int32> {
        if(b == 0) return None;
        else return Some(a/b);
    }

    pub fn operation(a: int32, b: int32) -> Result<int32, DivError> {
        let r1 = div(a, b)
            .ok_or(DivError::DivByZero)?;
        let r2 = div(r1, 3);
        return r2;
    }

## Façade and Error layer between HTML and Rust/Wasm

With this is mind we will start to define how we are going to organize our façade and error layer.

We want all callable function to return a JsValue or an error. The error will be a negative int (as error code) or a string.

    pub enum WasmErrorCode
    {
        None
    }

    pub struct WasmError
    {
        code: WasmErrorCode,
        message: String
    }

    type WasmResult = Result<JsValue,JsValue>;

So with this we can start our fist function that will search for the canvas.

    #[wasm_bindgen]
    pub fn start_webgl(canvas_id: &str) -> JsResult {
        let document = web_sys::
            window().jsok()?
            .document().jsok()?;
        let canvas = document
            .get_element_by_id(canvas_id)
            .jsok_msg(format!("canvas not found: id=[{}]", canvas_id))?;
        return Ok(JsValue::from(1));
    }

And our HTML

    <html>
        <body>    
            <script type="module">
                import init, { start_webgl } from './wasm/rust_webgl.js';
                async function start()
                {
                    await init();
                    var r = start_webgl("screen");
                    console.log(r);
                }
                start();
            </script>
        </body>
    </html>

This will generate the error, because the canvas does not exists. We can fix this just inserting the canvas before the script

    ...
    <body>    
        <canvas id="screen"></canvas>
        <script type="module">
    ...

Now we can create the WebGL context.

    use web_sys::{HtmlCanvasElement, WebGlRenderingContext};
    #[wasm_bindgen]
    pub fn start_webgl(canvas_id: &str) -> JsResult {
        ...
        let canvas = document
            .get_element_by_id(canvas_id)
            .jsok_msg(format!("canvas not found: id=[{}]", canvas_id))?
            .dyn_into::<HtmlCanvasElement>()?;

        let gl = canvas
            .get_context("webgl")?
            .jsok_msg(format!("webgl not supported: id=[{}]", canvas_id))?
            .dyn_into::<WebGlRenderingContext>()?;

        gl.clear_color(0.0, 0.0, 0.0, 1.0);
        gl.clear(WebGlRenderingContext::COLOR_BUFFER_BIT);

        return Ok(JsValue::from(1));
    }

And now we have our black screen. Now let us go to our loop.

## Loop

To fully understand the loop code we need to understand a littl bit more of Rust.

### Anonymous functions

    let renderf = || {
    };

### Closure

### Move anonymous function

The only difference here is that the coluse will "move" every capture object to the anonymous function.

    let renderf = move || {
    };

### Box

    Box::new(renderf)

### Box with dynamic disptach

    Box::new(renderf) as Box<dyn FnMut()>

### Fn, FnMut, FnOne

Se more at: 
Rust Guide - Capturing the Environment with Closures


### Rc + RefCell

    *g.borrow_mut() = Some(...)

Equals to 

    *((&g).borrow_mut()) = Some(...)

Which actually is:

    let temp1 = &g;
    let mut temp2 = temp1.borrow_mut();
    *(temp2) = Some(...);

Rust Guid - Where’s the -> Operator?
Rust Guide - Having Multiple Owners of Mutable Data by Combining Rc<T> and RefCell<T>

### js-sys and heap Closures


JSSYS Guide - http://localhost:3000/reference/passing-rust-closures-to-js.html