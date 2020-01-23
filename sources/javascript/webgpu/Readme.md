# WebGPU

If you are old enough and stop to think you will realize that 3D APIs have more two decades now. And they have evolved.

A new generation is emerging: Vulkan, Metal and WebGPU.

They  all have tha same philosophy: less intermediary code in drivers and more direct control to hardware.

This allows your code to keep the GPU busy delivering more frames or better graphics; but demanding more of the developer.

Here we will, first, dive into WebGPU. Running directly from the browser and having a javascript API it is the simpler to configure.

# Our objective in this tutorial

![Vertices](images/Annotation&#32;2020-01-23&#32;083858.png?raw=true)

# How 3d graphics are rendered

It almost impossible to not get lost in the setup of these 3 APIs if you do not have, at least a basic understanding of how 3D graphics become the images that we see. Or even animations.

## 2D Images

The first point is how 2D images are stored in computers. Images are composed of minors parts called pixels forming a grid. This is almost universally known nowadays. But when configuring these 3D APIs you have various parameters as to how these pixels are stored in the GPU memory.

You have to decide how many bits per pixel, for example. If you have a alpha channel or not, used for transparency. So it is not uncommon to have 32bits per pixel. We normally reserve  eight bits (one byte) for each color channel, so: R8G8B8A8. 

What is a little bit more exotic is that you can have more than one color information per pixel. Without more context this sounds like a very stupid idea. But is very useful, actually. Remember that when rendering a 3D image or a game, you are drawing a lot of thing one on top of the other: first the very far away mountain, a house and then your character. Being able to have more than one color per pixel allows edges to be mixed and eliminate a pixelization effect. This is called multisampling. The name comes from you sample multiple colors for each pixel. In theory the mor subsamples you have per pixel the better, but they cost more.

## Double Buffering

Everyone also know tht we achieve animation by exchange very similar image. The same technique is achieved in 3D using just two images(off course, you can use more if you want). When you have two images this is called double buffering swap chain.

One of the first tasks in these APIs is to configure your swap chain. We will configure two images and each time that we render we will be displaying one image and be rendering to the another.

## 3D Data

Since Descartes we define points in 3D with three numbers, its coordinates. Here we will do the same. We will render a triangle, three points, nine coordinates.

The hard part comes that we need, first, to send these coordinates to the GPU memory. This looks like a lot of work, but these APIs are made to much more heavy usage. Later we will devise a easier approach.

## Spaces

This is the most esoteric topic, by far. Specially if math, linear algebra, scares you. We have two mains problems here:

1 - We will define 3D points, but pixels are in 2D;
2 - The 3D points can be any float but pixels to be visible need to be between (0,0), (800,600) for example.

The first problem is solve with some complex matrices mathematics, I grant you. You do not need to understand everything now. You just need to understand that we will transform any three floats in any two floats. From 3D to 2D. From R^3 to R^2. 

<TODO>DEPTH BUFFER
<TODO>STENCIL BUFFER

Then we need to transform any two floats to our valid range: (0,0)x(800,600), for example. Internally this is also done by mathematics, but for the API this will be configuration for us. Our valid range is controlled by two configurations:

1 - Viewport
2 - Scissors

Scissors will "cut" every pixel that after the transformation lies outside its range. Viewports will determine how 2D float between (-1,-1)x(1,1) will be transformed. We want (-1,-,1) to be pixel (0,0); and (1,1) to be pixel (800,600).

## Rasterization

![Vertices](images/d3d10-rasterrulesline.png?raw=true)
![Vertices](images/d3d10-rasterrulestriangle.png?raw=true)

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