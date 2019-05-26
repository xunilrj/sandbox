# webint 13H

In the good old days of Computer Graphics all developers had access tho a contiguous memory array and writing a pixel as as simple as just MOVing a value to memory. Then came the "layers". Windows, DirectX, OpenGL etc... and the contiguous memory vanished. But we can bring it back!

Off course I am being a little bit overdramatic. We can still do this in theses abstractions. And we ARE going to do this here in webGL + WASM.

But first, let us reminder ourselves how things used to work:

    #include <go32.h>
    #include <sys/farptr.h>
    
    void putpixel_13h(int x, int y, int color)
    {
        _farpokeb(_dos_ds, 0xA0000+y*320+x, color);
    }
    http://www.delorie.com/djgpp/doc/ug/graphics/vga.html

This function will write pixel "color" in the "x" and "y" coordinates. The "0xA0000" is the standard address to access the VGA. In this case the function is hardcoded to 320x240. The important part is how to map from (x,y) to a contiguous memory array: using the function (x,y) -> start+(y*width)+x.

    The video memory of the VGA is mapped to the PC's memory via a window in the range between segments 0xA0000 and 0xBFFFF in the PC's real mode address space (A000:0000 and B000:FFFF in segment:offset notation). Typically, these starting segments are:
    0xA0000 for EGA/VGA graphics modes (64 KB)
    (https://en.wikipedia.org/wiki/Video_Graphics_Array)

To good news is that we can easily do this in every framework that exists: win32, GDI, DirectX, OpenGL, SDL etc... you name it, and we can do it. To prove, we are goind to do it inside webGL. Allowing us to write "directly" to the screen using Javascript and WASM. "Directly" was in quotes because, in reality, is as "directly" as possible. But the feeling is exactly the same.

So let us begin!

# Setup

Our first step is the start out HTML page and initialize WebGL. This tutorial does not intend to be an introduction to WebGL/OpenGL and/or 3D development, altough we will explain everything. WebGL is acessed throught the "canvas" element. We just need to get the "webgl" context. Much simpler than in Windows/Linux environment.

    <!DOCTYPE html>
    <html>
    <body>
        <canvas id="screen" width="320" height="240"></canvas>
        <script>
            var canvas = document.getElementsById("screen");
            var gl = canvas.getContext("webgl");
        </script>
    </body>
    </html>

What we need to do now is not very intuitive. WebGL/OpenGL is aimed to drawing geometric figures, specially in 3D. What we want is a contiguous memory address to write our pixels. The easist way to achieve this in WebGL is:

1 - Draw a rectangle that fills 100% of the screen;
2 - Apply a texture that is exactly the same size as the screen to this rectangle;
3 - Get the address of this texture;
4 - Write what we want to draw in this address;
5 - Update the texture;
6 - Update the screen;

Steps 1,2,3 are setup and done only once in the initialization. Steps 5, 6 are generic. And step 4 is our code. What we will do is pass the texture address to a function that will ignore everything and just write to this memory.

## Step 1 - Rectangle

WebGL/OpenGL aim is much complex cases than drawing rects. So it looks like a lot of work to draw a simple rectangle. But remember that we need to send everything to the video card, and then ask the WebGL/OpenGL/Video Card to draw what we created. So that is what we need to do: 

1 - specify each vertice in a rectangle (there are four of them);
2 - specify the triangles that forms the rectangle (WebGL/OpenGL love triangles) (there are two triangles);
3 - draw them.

The vertices are easy to understand. A rectangle contains four vertices. WebGL starts with the point (0,0) right in the middle of the screen with the top-left pixel being the (-1,-1) and the bottom right being (1,1).

The triangles are specified by three integers that points to the vertice array.

WebGL/OpenGL works always like this:

1 - Create an object. You receive a integer that "points" to the object (remember the object lives inside the Video Card);
2 - Activate the buffer (tells WebGL/OpenGL that you will modify this object)

We do not need to return the array with vertices and indices, because we copyed them to the video card. That is what the "verticesBuffer" and "indicesBuffer" mean.

    function createRectangle(){
        // Vertices
        var positions = new Float32Array([
            -1.0, -1.0,
            +1.0, -1.0,
            +1.0, +1.0,
            -1.0, +1.0,
        ]);
        var verticesBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, verticesBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);

        // Triangles
        var indices = new Uint16Array([0, 1, 2, 0, 2, 3]);
        var indicesBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indicesBuffer);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);
        return [verticesBuffer, indicesBuffer];
    }

    function draw(verticiesBuffer, indicesBuffer) {    
        gl.bindBuffer(gl.ARRAY_BUFFER, verticiesBuffer);
        gl.vertexAttribPointer(a_coords, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(a_coords);

        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indicesBuffer);
        gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);
    }

The draw code contains some mysterious variables, that we have not seen yet, but the general "geist" of the code is obvious. Activate the vertex buffer, the index buffer and them webGL to use them. This will give us a rectangle filling the entire screen.

## Step 1 - Apply Texture

So our next step is to apply the texture. To apply a texture you need two things. A texture, a image and associate to each vertex a coordinate in the image. If you say that vertex 0 has texture coordinates (0,0) that means that when drawing that vertex, you draw the first pixel of the image. WebGL will interpolate the value between vertices. So our function now is:

We will start with the second step. The texture coordinates creation.

    function createRectangle(){
        // Vertices
        var positions = new Float32Array([
            -1.0, -1.0,
            +1.0, -1.0,
            +1.0, +1.0,
            -1.0, +1.0,
        ]);
        var verticesBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, verticesBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);

        // Texture
        var texCoords = new Float32Array([
            +0.0, +0.0,
            +1.0, +0.0,
            +1.0, +1.0,
            +0.0, +1.0,
        ]);
        var texCoordsBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, texCoordsBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, texCoords, gl.STATIC_DRAW);

        // Triangles
        var indices = new Uint16Array([0, 1, 2, 0, 2, 3]);
        var indicesBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indicesBuffer);
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);
        return [verticesBuffer, texCoordsBuffer, indicesBuffer];
    }

    function draw(verticiesBuffer, texCoordBuffer, indicesBuffer) {
        // activate vertices
        gl.bindBuffer(gl.ARRAY_BUFFER, verticiesBuffer);
        gl.vertexAttribPointer(a_coords, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(a_coords);

        // activate texture
        gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
        gl.vertexAttribPointer(a_texcoords, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(a_texcoords);

        // draw triangles
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indicesBuffer);
        gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);
    }
 
Now we create the texture. Normally one would read an image file like .bmp, .png etc... We will actually create an empty image. Because we want a way to write pixels and send to the video card. The way to do this is very simple. This function is going to receive the buffer, instead of creating, because we will use an existing buffer later on.

There are a lot of parameters and details that we will skip here at this moment. The important thing here is how we create the texture and send the "buffer" to the video card and the texture pixels. We will do something very similar later on to update the texture.

    function createEmptyTexture(width, height, buffer) {
        var pixels = new Uint8Array(buffer);     

        var texture = gl.createTexture();        
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, pixels);

        return [texture, pixels];
    }

    function draw(texture, verticiesBuffer, texCoordBuffer, indicesBuffer) {
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, texture);

        ... //same as before
    }

Now our rectangle has a texture. Given that out texture is empty, our rectangle is also empty. That is ok. because we are about to write something to it.

# C/C++ Code

    #define EXPORT __attribute__((visibility("default")))

    void set(int* ptr) asm("set");

    #define i8 char
    #define i16 short
    #define i32 int
    #define ui8 unsigned char
    #define ui16 unsigned short
    #define ui32 unsigned int
    #define RGBA(r, g, b, a) (((ui8)(a)) << 24 | ((ui16)((ui8)(b)) << 16) | (((ui16)(ui8)(g)) << 8) | (((ui16)(ui8)(r)) << 0))
    int seed = 123456789;
    int m = 0x8000;
    int a = 1103515245;
    int c = 12345;
    int rand()
    {
        seed = (a * seed + c) % m;
        return seed;
    }

    const int w = 100;
    const int h = 100;
    i32 screen[w*h];

    EXPORT
    void set(int* ptr)
    {
        for(int x = 0;x < w;++x){
            for(int y = 0;y < h; ++y){
                ptr[y*w+x] = RGBA(
                    rand() % 255, 
                    rand() % 255,
                    rand() % 255,
                    255);
            }
        }
    }

    EXPORT
    int main()
    {
        return 14;
    }


# HTML

    <!DOCTYPE html>
    <html>
    <head>
    <meta charset='utf-8'>
    <style>
        body {
            background-color: white;
        }
    </style>
    </head>
    <body>
    <div>
        <canvas width="100" height="100"></canvas>
    </div>
    <span id="container"></span>
    <span id="fps"></span>
    <script src="./main.js"></script>
    <script id="vshader" type="webgl/vertexshader">
            attribute vec2 a_coords;
            attribute vec2 a_texcoords;

            varying highp vec2 out_texcoords;
            void main() {
            gl_Position = vec4(a_coords, 0.0, 1.0);
            out_texcoords = a_texcoords;
            }
        </script>
        <script id="fshader" type="webgl/fragmentshader">
            precision mediump float;

            uniform sampler2D u_texture0;
            varying highp vec2 out_texcoords;

            void main() {
            gl_FragColor = texture2D(u_texture0, out_texcoords);
            }
        </script>
        <script>
            startGL.then(function () {
                var canvas = document.getElementsByTagName("canvas")[0];
                var gl = canvas.getContext("webgl");

                /////////////////////////////////////////////// SHADERS
                var vshader = document.getElementById("vshader").innerText;
                var fshader = document.getElementById("fshader").innerText;
                var vsshader = gl.createShader(gl.VERTEX_SHADER);
                gl.shaderSource(vsshader, vshader);
                gl.compileShader(vsshader);
                if (!gl.getShaderParameter(vsshader, gl.COMPILE_STATUS)) {
                    console.log(gl.getShaderInfoLog(vsshader));
                }
                var fsshader = gl.createShader(gl.FRAGMENT_SHADER);
                gl.shaderSource(fsshader, fshader);
                gl.compileShader(fsshader);
                if (!gl.getShaderParameter(fsshader, gl.COMPILE_STATUS)) {
                    console.log(gl.getShaderInfoLog(fsshader));
                }

                var shaderProgram = gl.createProgram();
                gl.attachShader(shaderProgram, vsshader);
                gl.attachShader(shaderProgram, fsshader);
                gl.linkProgram(shaderProgram);
                var a_coords = gl.getAttribLocation(shaderProgram, 'a_coords');
                var a_texcoords = gl.getAttribLocation(shaderProgram, 'a_texcoords');
                var u_texture0 = gl.getUniformLocation(shaderProgram, 'u_texture0');

                ////////////////////////////////////////////// TEXTURE
                var level = 0;
                var internalFormat = gl.RGBA;
                var width = 100;
                var height = 100;
                var border = 0;
                var srcFormat = gl.RGBA;
                var srcType = gl.UNSIGNED_BYTE;
                var texture = gl.createTexture();
                gl.bindTexture(gl.TEXTURE_2D, texture);
                gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
                gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
                gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
                gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
                    width, height, border, srcFormat, srcType,
                    pixel);
                console.log(gl.getError());
                ////////////////////////////////////////// VERTICES
                var positions = new Float32Array([
                    -1.0, -1.0,
                    +1.0, -1.0,
                    +1.0, +1.0,
                    -1.0, +1.0,
                ]);
                var positionBuffer = gl.createBuffer();
                gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
                gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STATIC_DRAW);

                var texCoord = new Float32Array([
                    +0.0, +0.0,
                    +1.0, +0.0,
                    +1.0, +1.0,
                    +0.0, +1.0,
                ]);
                var texCoordBuffer = gl.createBuffer();
                gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
                gl.bufferData(gl.ARRAY_BUFFER, texCoord, gl.STATIC_DRAW);

                var indices = new Uint16Array([0, 1, 2, 0, 2, 3]);
                var indexBuffer = gl.createBuffer();
                gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
                gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, indices, gl.STATIC_DRAW);

                ///////////////////////////////////////// DRAW
                var fps = document.getElementById("fps");
                var lastTimestamp = null;
                var i = Math.random() * 3;
                var maxDT = 0;
                var maxDTi = 0;
                function render(timestamp) {
                    if (!lastTimestamp) lastTimestamp = timestamp - 16;
                    var dt = (timestamp - lastTimestamp) * 0.001;
                    maxDT += dt;
                    maxDTi++;
                    lastTimestamp = timestamp;

                    if ((timestamp % 10) == 0) {
                        i = Math.random() * 3;
                    }

                    var txt = "";
                    try {
                        if (instance.exports) {
                            instance.exports.set(pixel.buffer);
                        }
                    } catch (e) {
                        console.log("error",e);
                    }

                    gl.bindTexture(gl.TEXTURE_2D, texture);
                    gl.texImage2D(gl.TEXTURE_2D, level, internalFormat,
                        width, height, border, srcFormat, srcType,
                        pixel);
                    //fps.innerHTML = gl.getError()
                    gl.clearColor(0.0, 0.0, 0.0, 1.0);
                    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

                    gl.useProgram(shaderProgram);
                    gl.activeTexture(gl.TEXTURE0);
                    gl.bindTexture(gl.TEXTURE_2D, texture);
                    gl.uniform1i(u_texture0, 0);

                    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
                    gl.vertexAttribPointer(a_coords, 2, gl.FLOAT, false, 0, 0);
                    gl.enableVertexAttribArray(a_coords);

                    gl.bindBuffer(gl.ARRAY_BUFFER, texCoordBuffer);
                    gl.vertexAttribPointer(a_texcoords, 2, gl.FLOAT, false, 0, 0);
                    gl.enableVertexAttribArray(a_texcoords);

                    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indexBuffer);
                    gl.drawElements(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0);

                    fps.innerHTML = `${dt.toFixed(4)} - max: ${(maxDT / maxDTi).toFixed(4)} - fps ${(1 / dt).toFixed(4)} `;
                    requestAnimationFrame(render);

                };
                requestAnimationFrame(render);
            });
        </script>
    </body>
    </html>

# Javascript

    let x = '../out/main.wasm';

    let instance = null;
    let memoryStates = new WeakMap();

    let pixel;
    let startGLR;
    let startGL = new Promise((a,b) => {
    startGLR = a;
    });
    function syscall(instance, n, args) {
    switch (n) {
        default:
        // console.log("Syscall " + n + " NYI.");
        break;
        case /* brk */ 45: return 0;
        case /* writev */ 146:
        return instance.exports.writev_c(args[0], args[1], args[2]);
        case /* mmap2 */ 192:
        debugger;
        const memory = instance.exports.memory;
        let memoryState = memoryStates.get(instance);
        const requested = args[1];
        if (!memoryState) {
            memoryState = {
            object: memory,
            currentPosition: memory.buffer.byteLength,
            };
            memoryStates.set(instance, memoryState);
        }
        let cur = memoryState.currentPosition;
        if (cur + requested > memory.buffer.byteLength) {
            const need = Math.ceil((cur + requested - memory.buffer.byteLength) / 65536);
            memory.grow(need);
        }
        memoryState.currentPosition += requested;
        return cur;
    }
    }

    let s = "";
    fetch(x).then(response =>
    response.arrayBuffer()
    ).then(bytes =>
    WebAssembly.instantiate(bytes, {
        env: {
        __syscall0: function __syscall0(n) { return syscall(instance, n, []); },
        __syscall1: function __syscall1(n, a) { return syscall(instance, n, [a]); },
        __syscall2: function __syscall2(n, a, b) { return syscall(instance, n, [a, b]); },
        __syscall3: function __syscall3(n, a, b, c) { return syscall(instance, n, [a, b, c]); },
        __syscall4: function __syscall4(n, a, b, c, d) { return syscall(instance, n, [a, b, c, d]); },
        __syscall5: function __syscall5(n, a, b, c, d, e) { return syscall(instance, n, [a, b, c, d, e]); },
        __syscall6: function __syscall6(n, a, b, c, d, e, f) { return syscall(instance, n, [a, b, c, d, e, f]); },
        putc_js: function (c) {
            c = String.fromCharCode(c);
            if (c == "\n") {
            console.log(s);
            s = "";
            } else {
            s += c;
            }
        }
        }
    })
    ).then(results => {
    instance = results.instance;
    document.getElementById("container").innerText = instance.exports.main();

    var mem = instance.exports.memory;
    pixel = new Uint8Array(mem.buffer);                
        
    try {
        instance.exports.set(pixel.buffer);
    } catch (e) {
        console.log("error", e);
    }

    startGLR();
    }).catch(console.error);