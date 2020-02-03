# WebGPU

If you are old enough and stop to think, you will realize that 3D APIs have more two decades now. OpenGL, the most famous, has almost 30 years. The OpenGL Red Book is already on its 9th version. Boy, time does fly!

But we do not intend to get nostalgic here, our intention is to understand the new philosophy behind the new generation of 3D APIs. If you have any previous experience, they are all welcomed, but the APIs are very different from the earlier version. They have evolved a lot!

This new generation is amidst its peak now: Vulkan, Metal and webGPU, they are all gaining market share as we speak. Well, actually, webGPU is still in alpha, but it is so similar to others, that is worth a try already. And that is precisely what we are going to do here.

The first difference from earlier APIS that we must perceive is a different philosophy: thinner APIs, with less intermediary code in drivers and more direct control of the hardware.

This allows your code to keep the GPU busy delivering more frames or better graphics but demanding more of the developer. There a lot of configuration and even a small "hello triangle" code has hundreds of lines of code.

Here we will, first, dive into webGPU. Running directly from the browser and having a javascript API, makes it the perfect choice as the first contact with this new generation.

# Our objective in this tutorial

In this introductory tutorial, we will render a simple coloured triangle as the image below.

If you have experience in 3D, feel free to jump directly to the [Code](#Code) section. If you are totally new, you can continue reading and follow a very brief explanation of the convoluted process of generating 2D images from 3D worlds.

![Vertices](images/Annotation&#32;2020-01-23&#32;083858.png?raw=true)

# How 3d graphics are rendered

It is almost impossible not to get lost in the setup of these new APIs if you do not have, at least, a basic understanding of how 3D worlds become the images that we see.

To have the necessary understanding of all the process, you somehow need to understand:

1 - How 2D images are stored in a computer;  
2 - How you can generate animation with a set of images;  
3 - How 3D worlds and objects are stored in a computer;  
4 - Bare minimum of linear algebra;  
5 - Algorithms to draw lines and triangles.  

We will see each of these topics very briefly, just to give you the necessary to understand the configuration of the [Code](#Code) section.

## 2D Images

2D images in computers are composed of a grid of colours. If we zoom on our triangle image, we will see this grid. Each square in this grid is what we call a "pixel".

![Vertices](images/pixelgrid.png?raw=true)

We can store each pixel pretty much the way we prefer: we can use just one bit and have "binary images" or "monochrome bitmaps"; up to 32 bits per pixel. This is the more common approach and what we will use.

Monochrome Bitmaps  
http://www.fastgraph.com/help/monochrome_bitmaps.html  

If you dig a image composition software you will probably see a image like the one below. This image helps us to understand how the pixels are stored in a 32bit-per-pixel image.

Take a look at the RGB values and the Alpha at the bottom. These are called channels, we store colours as a composition of these four values. 

![Vertices](images/pixelstored.png?raw=true)

If you glance at the image again, you will realize that R is zero, and the slider is at the beginning. Alpha is 255, and the slider is at the end. This is not a coincidence. Each of these values has a valid range of 0 to 255. One byte. Four values, four bytes. 32 bits.

So in the end, images can be stored in a computer as simple arrays of 32 bits integers.

```c++
auto img = new uint32_t [800 * 600];
```

```js
const img = new Uint32Array(800 * 600);
```

For this reason, is common to address colours as 32 bits hexadecimals numbers. Take even another glance at the image and search the "Hex" box. You will see "004CB2". Every two digits correspond as the hexadecimal value of that channel. In an RGB image, this means: 00 is the Red, 4C is the Green and B2 is the blue channel. 

We need to store the Alpha, and you can put at the end, for example, creating a pixel as RGBA and its hexadecimal value as "004CB2FF".

There is no reason to have RGBA as the order instead of, let us say, BGRA. In the end, it makes no difference, and some software or APIs do make this change.

These new APIs like to be very specific, so when you ask the GPU to create an image for you, you need to give even for details, that is why the image description sometimes is "R8G8B8A8". Where the number specifies the size in bits of that channel.

## Animation

The first time that some wrote of writing a machine that would generate what we would today call animation is the paper "Account of an Optical Deception" of 1821. The author very artfully perceives that:

"an impression made by a pencil of rays on the retina, if sufficiently vivid, will remain for a certain time after the cause has ceased."

This is the idea behind the concept of "Persistence of Vision", which has a lot of critics since 1912, when started the Phi/Beta interpretation.

Account of an Optical Deception  
https://archive.org/stream/quarterlyjournal10roya#page/282/mode/2up  

Phi is not beta, and why Wertheimer's discovery launched the Gestalt revolution  
https://www.sciencedirect.com/science/article/pii/S0042698900000869  

To us here matters only the conclusion that if we flip very similar images, around twelve per second, we achieve the perception of animation and not the perception of similar-images-flipped. Each new similar-image is called a "frame" of the animation.

Thus, to animate, we need a set of frames, and we need to display these frames following a particular clock tick, for twelve frames-per-second (FPS) we need to update the displayed image every 83 milliseconds (ms), to generate the illusion of the animation.

Nowadays it is expected 60 FPS, or display a new frame every 16 ms.

In practice, what we do is the following: we have two frames. The first is being displayed on the monitor. The second is "hidden", and we will be rendering the next frame of the animation in this frame. In the correct time, we swap them. The second image goes to the monitor, and the first become hidden for 16 ms, the timeframe we have to render the next frame on it.

This is called double buffering.

The image below represents what would happen in double buffer scenario where the rendering code always takes less than 16 ms.

![Vertices](images/swapok.png?raw=true)

On the other side, if the rendering code takes more than 16 ms, we would miss the "swap tick" and give the impression of "only" 30 FPS, because the current image would be visible for 32 ms, instead of 16 ms.

![Vertices](images/swaplost.png?raw=true)

If your animation also contains input, maybe feedbacks of the animation like games, this "FPS drop" can mean a delay of dozens of ms and really kill the "real-time experience" given, but this comes to be problems related to what is generally called "game loop", we will more of this later.

## 3D Data

I think the best way to describe how we store 3D data in computers today is to start thinking about flies. Yeah, that annoying little bug. Sound strange? It is precisely how it started, or at least how tales tell us.

Tales tell that in cold French night of the 17th century, Descartes was trying to sleep when he spotted a fly in the ceiling. Being as crazy as any other thinker, he asked himself what would be the best way to describe the fly position. 

The corner of the ceiling would be an excellent reference point. So the fly position would be the number of centimeters (cm) to one of the walls, and the number of centimeters to the other wall (ok, the metric system would not be used in Decarte's time, let us forgive this anachronism).

So Descartes could have said that the fly was 244 cm of the right wall and 249 cm of the front wall.

![Vertices](images/coordinates.gif?raw=true)

One little trick in Mathematics is that if you do not know how to name something, name it with a letter. So the distance away from the right wall, we call it "x"; and the distance from the front wall we call it "y". 

So the fly position is 244cm in the "x" direction, and 249 in the "y" direction, or abbreviated as (244, 249) and called the fly coordinates. The convention is "x" first, and "y" later. Now we have various other coordinates conventions.

Some say that this tale is a myth, in the sense that is not true, but IT IS a myth, in the sense that is powerful. Especially for computer graphics.

Descartes would never imagine how computers work today, but being able to specify a position in 2D space like this is very easy. We need just two "float"s.

```c++
struct vec2
{
    float x;
    float y;
};
```

And one of the beautiful aspects of this is that to store flies positions in 3D, we need another distance that we call "z", the distance from the ceiling.

```c++
struct vec3
{
    float x;
    float y;
    float z;
};
```

And to store an array of points, we can create an array of this struct. That would be packed in memory and would enable us to access any position and any coordinate with a simple memory offset.

```c++
auto points = new vec3[10];
```

For example, we know that the first point coordinates is at the start of the array, at offset 0. The second point coordinate is at offset 12. The third at 24. Forming an arithmetic sequence: 0, 12, 24, 36, 48 etc...

The difference between two succeeding offsets is always the same, 12 bytes, the size of the "vec3", we call this the "stride", a synonym for "step" for non-native English speakers, because is how much you need to "walk" to find the next 3d position.

![Vertices](images/3darraymem.png?raw=true)

## Spaces

Spaces are by far one the most abstract concepts that we need to understand in 3D development. I am not saying that they are hard because it is not the truth, but we will need a little effort to understand them.

Altough they come from abstract linear algebra foundations, here we only need its very basics:

1 - reference points;  
2 - new directions (new basis).

Remember Descarte's fly? The ceiling corner was the "reference point", "x" was the direction away from one of the walls. 


Now image that tin the middle of the ceiling there is a lamp. Descartes could have chosen this point as the reference point. So, if we change the "reference point" from the corner to the lamp position, the "fly" coordinates will have to change, because we are measuring now distances from the lamp. But to what? How do we calculate this? In this simple case, we can "translate", move all points accordingly.

![Vertices](images/newcoord.gif?raw=true)

So in this case the fly is at (-56,-51). We call this a "transformation" and in this particular case "translation".

But suppose that we also change the direction we measure the distance. Now, we want to measure the distance along the direction of the lamp to the upper-right corner. This will be the "new x". And the direction along the lamp to upper-left will be the "new y". But how do we calculate the fly coordinates now?

![Vertices](images/newcoord_newdir.gif?raw=true)

First, let us reason again how we calculated the old coordinate. One can say that the fly walked 244 cm along the "x" direction, and then it "walked" 249 cm in the "y" direction. 

It does make sense. Let us try the same technique to calculate new coordinate and then reason if it makes sense. In the image below, we "walk" a certain amount of centimeters along the "new x", and then along the "new y".

![Vertices](images/walkcoord.gif?raw=true)

The new "y" coordinate was very small because the fly is almost at the new "x" direction. Let us zoom to see what is happening. We are also going to start with the "y" direction now.

![Vertices](images/walkcoordzoom.gif?raw=true)

You can see that we always walk 107/2 cm in the opposite direction of the new "x" direction (from the lamp to the corner) and 5/2 cm in the new "y" direction. It does not matter which direction we start. We always arrive at the fly position.

So there you have it! This is the new position of the fly in this new "coordinate system": (-107/2, -5/2). We call this transformation: "basis change". "Basis" here is the set of directions that we use to "walk".

So let recap and understand the concept of "space" here.

The fly, when we take the corner as the "reference point" is at (244, 249). When the change the reference point to the lamp and measure the distance in another direction, the fly position is (-107/2, -5/2).

The question is... has the fly moved? No! This is NOT what we did here. The fly is immovable. We just changed how we measure its position. In the first case, we can say that the fly is at (244,249) at the "corner space". And we can also say that it is at (-107/2, -5/2) at the "lamp space". 

You can almost ways replace "lamp space" here with "from the lamp point of view".

How is this applied in 3D development? Everywhere!

When you specify a cube, for example, all the points are in "cube-center space". But if we want the render two cubes, one on top of the other, we need to put our first cube centred at (0,0,0) and the other at (0,1,0) for example.

Something like:

```js
drawCubeAt(0,0,0);
drawCubeAt(0,1,0);
```

The API will, with our help, transform all the cube points from the "cube-center space", actually called as "object space" to "world space". In this simple case, we will increase by one the "y" coordinate of each point.

Let us return to our fly example. Suppose that our fly is specified as just one point". The point is defined in "model space" as (0,0,0).

```js
auto flyPoints = {0, 0, 0};
```

Our "world" will have two flies. So we will do something like:

```c++
void drawFly(float worldX, worldY, worldZ)
{
    auto flyPoints = {0, 0, 0};
    drawPointAt(
        flyPoints.x + worldX,
        flyPoints.y + worldY,
        flyPoints.z + worldZ);
}

drawFly(0,0,0);
drawFly(0,1,0);
```

You can imagine that instead of just one point, a "drawCube" would have eight points. But we would follow precisely the same strategy.

The Mathematics magic is that we do these transformations using Matrices. We will see them in more detail later.

## Rasterization

Rasterization is the algorithm that given triangles (or any other geometrical figure) convert them to pixels. But to understand where the algorithms start, we need to understand one more space: "clip space". 

As we saw above, we can go from one space to another using transformation. The rasterization algorithm starts when we transform everything we want to render from the "world space" (or "view space", we will see this one later) to a "clip space", where everything visible, must be "squished" inside a cube between (-1,-1,-1) and (1,1,1).

![Vertices](images/cam.gif?raw=true)

This means that we need to translate, scale and rotate all objects until they fit inside his imaginary cube. Once inside this imaginary cube, we can ignore its "z" position, flattening everything, and scaling and translating everything so that the coordinates nows match the positions of the pixels. This is "viewport space".

![Vertices](images/rasterizer01.gif?raw=true)

Now comes the easy part. All we need to do is "paint" the pixels inside the triangle. You can start with the naive (and useless) algorithm that checks every pixel if it is inside the triangle or not. And we will arrive at:

![Vertices](images/rasterizer02.gif?raw=true)

We will see later better algorithms.

# Quick recap how to generate 2D images from 3D data

Now we are ready to enter the last step to generate our lovely 2D images from 3D data. First a quick recap where we are:

1 - We defined out model (fly or cube) in "object space";  
2 - We transformed them somehow to "word space";  
3 - We transformed them somehow to "clip space";  
4 - The API transformed it to "viewport space";  
5 - Rasterizer generated 2D image.  

# Code

To start we need very little:

1 - Chromium (download at: https://www.chromium.org/getting-involved/download-chromium);  
2 - Enable webGPU;  
3 - Install ParcelJS (see https://parceljs.org/getting_started.html);  
4 - Create a HTML with a canvas and name it "index.html"
5 - Create a empty index.js
5 - run "parcel index.html"

```html
<html>
    <head>        
    </head>
    <body>
        <canvas id="screen" width="800" height="600">
        </canvas>
        <script src="./index.js"></script>
    </body>
</html>
```
## Enable webGPU

![Vertices](images/configchromium.png?raw=true)

## Start ParcelJS

![Vertices](images/startparcel.gif?raw=true)

## Setup webGPU

The very first steps of the webGPU return Promises, for this reason our setup function will by async. If you are using ParcelJS as I suggested you need to import the "babel-polyfill" npm package to make it work.

```js
import 'babel-polyfill';
async function setup()
{
}
setup();
```

Our first step is to check if the browser supports webGPU. We are also printing everything to the console to inspect what is returned.

```js
async function setup()
{
    // Check webGPU is supported
    if(!navigator.gpu) {
        console.error("webGPU not supported")
        throw "webGPU not supported";
    }

    // Request device
    const gpu = navigator.gpu;
    const adapter = await gpu.requestAdapter();
    const device = await adapter.requestDevice();
    console.log(navigator);
    console.log(adapter);
    console.log(device);
    ...
}
```

See more at:  
https://gpuweb.github.io/gpuweb/#requestadapter  
https://gpuweb.github.io/gpuweb/#gpuadapter  
https://gpuweb.github.io/gpuweb/#gpu-device    
https://gpuweb.github.io/gpuweb/#requestdevice  

## Setup Swap Chain

Here we are going to create a double buffering swapchain with 32 bits. The "unorm" means that we can write four floats representing RGBA colors to it and it will rearrange and normalize the colors. For example, a red in float (1,0,0,1) will become (0,0,255,255).

We will need the swapChainFormat below, so let just save it as a constant.

```js
async function setup()
{
    ...

    const swapChainFormat = "rgba8unorm";
    const canvas = document.getElementById("screen");
    const ctx = canvas.getContext('gpupresent')
    const swapchain = ctx.configureSwapChain({
        device: device,
        format: swapChainFormat
    });
    console.log(ctx);
    console.log(swapchain);
    ...
}
```

See more at:  
https://gpuweb.github.io/gpuweb/#dom-gpucanvascontext-configureswapchain  
https://gpuweb.github.io/gpuweb/#enumdef-gputextureformat  
https://gpuweb.github.io/gpuweb/#typedefdef-gputextureusageflags  

## Send Vertex data to GPU

Now we will send our data to the GPU memory. We already saw vertices. We are in the 3D world space here, but we have not configured the transformation from 3D to 2D, so we are actually ate the (-1,-1)x(1,1) space. That is why out first vertex is (1,-1,0) - bottom right; our second is (-1,-1) - bottom left; and our last is (0, 1, 0), top vertice.

But we also can specify the color of each vertex. Why, and how the color varies from one vertex to the other we will see later, in the rasterizer pass.

Out last buffer is the index. We specify in the "array of vertices" which form the triangles.

![Vertices](images/Vertices.png?raw=true)

```js
async function setup()
{
    ...
    const positionBuffer = createBuffer(new Float32Array([
         1.0, -1.0, 0.0, // 0
        -1.0, -1.0, 0.0, // 1 
         0.0,  1.0, 0.0, // 2
    ]), GPUBufferUsage.VERTEX);
    const colorBuffer = createBuffer(new Float32Array([
        1.0, 0.0, 0.0, // 0
        0.0, 1.0, 0.0, // 1
        0.0, 0.0, 1.0, // 2
    ]), GPUBufferUsage.VERTEX);
    const indexBuffer = createBuffer(new Uint16Array([ 
        0, 1, 2
    ]), GPUBufferUsage.INDEX);
    ...
}
```

The GIF below show how each of these parameters change the rendered triangle. If you really zoom the video you will see some glitches in form of points but they were caused by the GIF format, nothing to do with webGPU.

![Vertices](images/vertexatts.gif?raw=true)

## Render the triangle

We will jump a lot of configuration now, because it will be easier to understand them later. Now we need to the GPU a render command. The way to send commands to the GPU is grouping them and send everything together to a "rendering queue".

```js
function sendCommands(queue, backBufferView)
{
    const commands = [
        ...
    ];
    queue.submit(commands);
}
```

Now we generate a "Render Pass", that glues everything that the GPU needs to render whatever it needs to render.

First, we need to know choose a target image (1). Here we will be rendering things to the backbuffer, but we can render to any image we like. This is very useful as we will se in future tutorials.

"loadValue" means what the GPU must do with the value that is already on the target iamge and "load" means load the pixel value (2). This will be important later. Today you can just "load" ou pass a hardcoded color (3).

"storeOp" means what the GPU must do with the generated fragment and "store" means write it (4). You can choose "store" or "clear" and you cannot pass a color.

Some in summary we will "blend" what is stored in the target image with the pixel that we will generate. "blend" is a important word here and we will understand it better later.

```js
function triangleRenderPass(backBufferView)
{
    let colorAttachment = {
            attachment: backBufferView, // 1
            loadValue: "load",          // 2
            //or
            //loadValue: { r: 0, g: 0, b: 0, a: 1 }, // 3
            storeOp: 'store',           // 4
        };

    let commandEncoder = device.createCommandEncoder()
    let pass = commandEncoder.beginRenderPass({
        colorAttachments: [ colorAttachment ],
    });
    pass.setPipeline(pipeline);
    pass.setViewport(0, 0, canvas.width, canvas.height, 0, 1);
    pass.setScissorRect(0, 0, canvas.width, canvas.height);
    pass.setVertexBuffer(0, positionBuffer);
    pass.setVertexBuffer(1, colorBuffer);
    pass.setIndexBuffer(indexBuffer);
    pass.drawIndexed(3, 1, 0, 0, 0);
    pass.endPass();
    return commandEncoder.finish()
}
```

See more at:  
https://gpuweb.github.io/gpuweb/#dom-gpucommandencoder-beginrenderpass  
https://gpuweb.github.io/gpuweb/#dictdef-gpurenderpassdescriptor  
https://gpuweb.github.io/gpuweb/#dictdef-gpurenderpasscolorattachmentdescriptor  

## Setup the Pipeline

Now we gonna dive deep in each state of the pipeline. Dashed states are scriptable, solid states are configurable.

![Vertices](images/pipeline.none.png?raw=true)

1 - Input Assembler  
2 - Vertex Shader
3 - Tesselation 
4 - Geometry Shaders
5 - Rasterization
6 - Fragment Shader  
7 - Color Blending  

Each of these parts are sub-system of its own, so we gonna need a specific subpart for each.

```js
const pipeline = device.createRenderPipeline({
    primitiveTopology: 'triangle-list', vertexState,    // 1
    vertexStage, layout,                                // 2
    rasterizationState,                                 // 5
    fragmentState,                                      // 6
    colorStates: [ colorState ],                        // 7
});
```

https://gpuweb.github.io/gpuweb/#dom-gpudevice-createrenderpipeline  


### 1 - Input Assembler

![Vertices](images/pipeline.inputasm.png?raw=true)

The main task of the "Input Assembler" stage is the understand the "input buffers", mainly the "index buffer", the "position vertex buffer" and other extra buffers.

The "index buffer" as we saw above is just a raw list of "unsigned ints" that points to others arrays. You need to specify what they mean (1). For example, with the configuration below we are specifying that we will pass a "triangle list", or, every three indices specifies a triangle. Other possible options are:  "point-list", "line-list", "triangle-list" and others.

```js
const pipeline = device.createRenderPipeline({
    primitiveTopology: 'triangle-list'/*1*/, vertexState
    ...
});
```

The second task of the "Input Assembler" is to..., well, assemble the input buffers. Here we will not specify what buffers are needed to render the triangle. This was done above. 

Here we are specifying what buffers are needed. Imagine that here you are defining a function and above we were calling the function. If this picture in mind we are defining a pipeline that needs two buffers: a position buffer (1) and a color buffer (2).

As a last step we say that the index buffer is of "uint16" (3). So he knows that if have to jump 4 bytes to get to the next index.

```js
const vertexState = {
    indexFormat: 'uint16',  // 3
    vertexBuffers: [
        positionBufferDesc, // 1
        colorBufferDesc     // 2
    ]};
```

If we dive deeper on the buffers descriptions we will see that we define the "position buffer" as a "float3" (1) or a 3D Vector of floats, that starts at the beginning of the buffer (2) and we will use its value for the variable at position "0" of the vertex shader (3).

If you are wondering why so many parameters that seems useless, just wait because in future tutorials we will pass all render information in just one buffer. Easier to download everything, easier to setup.

Array stride (4) is the byte size need to find the next position. This array contain just positions and each position is 12 bytes.

We repeat everything for the color attribute, but the shaderLocation that now points to variable position 1 (5).

```js
const positionBufferDesc = {
    attributes: [
        { 
            format: 'float3',   // 1
            offset: 0,          // 2
            shaderLocation: 0,  // 3
        }
    ], arrayStride: 4 * 3,      // 4
};
const colorBufferDesc = {
    attributes: [
        { 
            format: 'float3'
            offset: 0,
            shaderLocation: 1   // 5
        }
    ], arrayStride: 4 * 3 };
```

See more at:
https://gpuweb.github.io/gpuweb/#enumdef-gpuprimitivetopology

### 2 - Vertex Shader

![Vertices](images/pipeline.vs.png?raw=true)

Vertex Shaders are one the most important an buzzed topics. They are "little" scripts that allows you to apply transformations to the vertex data.

In theory you could apply all changes that you need to the buffer and send them to the GPU again. In theory that works. The problem is that you will be suffer with the time to transfer all data necessary between the computer memory and the GPU memory.

If you are more ingenious you can think that instead of sending a lot of data, we could send the "algorithm" that change the data to the GPU. Yes, you can do this using "GPU Compute". We will do this in the future. But the far more common, and easier approach, and probably faster, is to use shaders.

In this case Vertex Shaders. Shader that will run for every vertex in this render pass.

Shaders remember any c-based language: c, c++, c#, java, javascript etc... The major difference here is that you specify your input and output as we do with "global variables".

Focus first at the variables with "layout (location = 0) in" (1) and "layout (location = 1) in" (2). These are the variables that will receive from the "Input Assembler" values at position 0 and 1, respectively. Go back to the shader configuration above and search for "shaderLocation: 0", for example. Nor you understand where the "vertex buffer" value will be used.

The unique mandatory output from a vertex shader is the "gl_Position" (3) that is the vertice position in 4D (ignore this for now). Just understand that "vec4(inPos.xyz, 1.0)" is the same as "vec4(inPos.x, inPos.y, inPos.z, 1.0)". We are just passing the position throught.

As additional data we also generating "outColor" (4). And again we are doing nothing and just passing it throught. The position of the output is important because it needs to match the input of variables in the fragment shader.

```glsl
layout (location = 0) in vec3 inPos;        // 1
layout (location = 1) in vec3 inColor;      // 2
layout (location = 0) out vec3 outColor;

void main()
{
    gl_Position = vec4(inPos.xyz, 1.0);     // 3
    outColor = inColor;                     // 4
}
```

The problem is that, out-of-the-box, webGPU only supports shaders in a binary format called SPV-R. And we have our shader here in "glsl". So we need to compile this shader using the tool "glslangValidator".

```
> glslangValidator.exe -V ./shaders/shader.vert -o ./shaders/vert.spv
```

To load them, after compilation, is very easy, but we need to download them. If you are using Parcel as I suggested, you can use a trick that when you import a unknown file, Parcel give you the URL to download that file. So we can do.

```js
import vertspv from './shaders/vert.spv';
async function loadSPV(url)
{
    const res = await fetch(url);
    const buffer = await res.arrayBuffer();
    return new Uint32Array(buffer);
}
const vertModule = device.createShaderModule({ code: await loadSPV(vertspv) });
const vertexStage = {
    module: vertModule,
    entryPoint: 'main'
};
```

Another good side from using Parcel is that it watches changes to its files. So if we change the spv file, by compiling the "shader.vert" file it will reload out little application. See below.

![Vertex Shader](images/vertexshader.gif?raw=true)

See more at:  
https://gpuweb.github.io/gpuweb/#dom-gpudevice-createshadermodule    
https://gpuweb.github.io/gpuweb/#dictdef-gpushadermoduledescriptor    
https://gpuweb.github.io/gpuweb/#typedefdef-gpushadercode  

### 3 and 4 - Tesselation and Geometry Shaders

![Vertex Shader](images/pipeline.tesselation.geom.na.png?raw=true)

Unfortunately these two stages are not yet available for webGPU, but they do exist in others APIs.

Tesselation exist to allow you to generate vertices inside the GPU. The idea is that generating these vertices inside the GPU is faster than moving them throught the GPU memory and vertex shader. This sound strange but we will see that the complexity of the vertex shader increases fast.

Another possible case is to have dynamic tesselation. Maybe you change the amount of vertices, and therefore the amount of detail bases on distance, for example.

A more complete history os hardware tesselation: 
http://rastergrid.com/blog/2010/09/history-of-hardware-tessellation.  

The geometry shader exist to run a "script" for each primitive. In our case here we would run a "script" for each triangle. It can also generate more vertices, but this is not its reason to exist. Geometry shaders should be used to:

- Layered rendering: rendering to multiple targets;  
- Transform feedback: save vertex data back to GPU memory.  

Just for the sake of completion this would be a Geometry Shader that does nothing. It just pass the vertices that it receives:

```glsl
layout(triangles) in;
layout(triangle_list, max_vertices = 3) out;

void main()
{
   for (int i = 0; i<gl_in.length(); i++)
   {
      gl_Position = gl_in[i].gl_Position;
      EmitVertex();
   }

   EndPrimitive();
}
```

### 4 - Rasterization

![Rasterization](images/pipeline.rast.png?raw=true)

When rendering volumes, like a cube, we are normally looking directly into a face but we are not viewing the faces in the other direction. Think of a dice. If you are seeing face 1, you cannot see face 6. But when rendering the dice we ask teh GPU to render all six faces. And we need to do everything with all the fragments of the face six that it is going to be overwritten by face one.

All of this sounds like a huge waste. And it is. One solution is, from the assumption that we will normally render volumes, we can choose one convention of specifying faces as clockwise, as the clock pointer rotates. See why we chose the vertices index as we did. Not imagine looking at this face from the back. From vertex 0, to 1, to 2 would be in counter-clockwise direction. So with this strange, but simple convention we have a simple way of defining if we are seeing a face from the front and from the behind.

Here we are configuring that when clockwise we are from the front (1) and that that we should cull, or just ignore all the "back" faces (2).

```js
const rasterizationState = {
    frontFace: 'cw', // 1
    cullMode: 'back' // 2
};
```

See more at:  
https://gpuweb.github.io/gpuweb/#dictdef-gpurasterizationstatedescriptor  

### 5 - Fragment Shader

![Vertex Shader](images/pipeline.fs.png?raw=true)

Fragment shaders, as Vertex Shaders are one the most important an buzzed topics. If in the "Vertex Shader" we wrote a little "script" for each vertex. Here we will write a small "script" for each fragment. Important no note, fragment are not pixels. At least not yet. But we will write pixels to the target image FROM fragments. Go back to the rasterization algorithm described above to remember the difference.

As the "Vertex Shader" we need to compile out fragment shader.

```
> glslangValidator.exe -V ./shaders/shader.frag -o ./shaders/frag.spv
```

And we load it exactly the same.

```glsl
const fragModule = device.createShaderModule({ code: await loadSPV(fragspv) });
const fragmentStage = {
    module: fragModule, 
    entryPoint: 'main'
};
```

Even the shaders are similar (luckily).

Not surprisingly we receive here what we outputted from the "Vertex Shader". The "outColor" there became the "inColor" here. Go look the "Vertex Shader" again to remember.

Here we are just passing through the received color, but the important point is, this happens for every fragment-that-will-become-pixel of the target. If our triangle in the end of all span 100 pixels, this "script" will be run 100 times.

```glsl
#version 450

layout (location = 0) in vec3 inColor;
layout (location = 0) out vec4 outFragColor;

void main()
{
  outFragColor = vec4(inColor, 1.0);
}
```

If you are using ParcelJS as I suggested you can very easily play with the fragment shader just compiling the .frag file. Like below. 

It is nor necessarily important to understand what is happening, but if you apply a weigth (the amount of energy) to each color channel, sum them and create a color with all channels equal this value, you create something like a "grayscale" filter. We will apply various "post-processing filters" like this one later.

![Vertex Shader](images/fragshader.gif?raw=true)

See more at:  
https://gpuweb.github.io/gpuweb/#dictdef-gpushadermoduledescriptor  

### 6 - Color Blending

![Vertex Shader](images/pipeline.cb.png?raw=true)

The color blending state allows us to configure how what we are rendering is going to be mixed with what already exist in the target image. For example, we are rendering a colored triangle in a black background. In this case we can control the final pixel color with the following formula.

```
Color_Result = ColorSource * colorBlend.srcFactor + ColorDestination * colorBlend.dstFactor;
Alpha_Result = AlphaSource * alphaBlend.srcFactor + AlphaDestination * alphaBlend.dstFactor;
```

Source is what we are drawing and destination is what is already on the image. We don't care for transparency for now. So we will simply use:

```js
const colorState = {
    format: 'bgra8unorm',
    alphaBlend: { srcFactor: 'one', dstFactor: 'zero', operation: 'add' },
    colorBlend: { srcFactor: 'one', dstFactor: 'zero', operation: 'add' },
    writeMask: GPUColorWrite.ALL
};
```

See more At:  
https://gpuweb.github.io/gpuweb/#dictdef-gpucolorstatedescriptor  