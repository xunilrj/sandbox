# WebGPU

If you are old enought and stop to think you will realize that 3D APIs have more two decades now. And they have evolved.

A new generation is emerging: Vulkan, Metal and WebGPU.

They  all have tha same philosophy: less intermediary code in drivers and more direct control to hardware.

This allows your code to keep the GPU busy delivering more frames or better graphics; but demanding more of the developer.

Here we will, first, dive into WebGPU. Running directly from the browser and having a javascript API it is the simpler to configure.

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

Since Decartes we define points in 3D with three numbers, its coordinates. Here we will do the same. We will render a triangle, three points, nine coordinates.

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

## Pipeline

Almost all the magic happens in what is called the pipeline. To do its magic it needs to know a lot of things, and you will see that there is a lot of configuration.

1 - Input Layout: define how your vertices are configured;
2 - Shaders
3 - Rasterizer
4 - Ouput Merger

# Code

```js
async function setup()
{
}
```


```js
async function setup()
{
    const gpu = navigator.gpu;
    const adapter = await gpu.requestAdapter();
    const device = await adapter.requestDevice();
}
```

The queue is one of the most important concepts. 

```js
const queue = device.defaultQueue;
```

```
const canvas = document.getElementById("screen");
const ctx = canvas.getContext('gpupresent')
const swapchain = ctx.configureSwapChain({
    device: device,
    format: 'bgra8unorm',
    usage: GPUTextureUsage.OUTPUT_ATTACHMENT |
        GPUTextureUsage.COPY_SRC
});
```

https://gpuweb.github.io/gpuweb/#dom-gpucanvascontext-configureswapchain
https://gpuweb.github.io/gpuweb/#enumdef-gputextureformat
https://gpuweb.github.io/gpuweb/#typedefdef-gputextureusageflags


Now we crate out depth texture. In this case we are creating:

- Depth buffer: 24 bits;
- Stencil buffer: 8 bits.

```
let depthTexture = device.createTexture({
    size: {
        width: canvas.width,
        height: canvas.height,
        depth: 1
    },
    arrayLayerCount: 1,
    mipLevelCount: 1,
    sampleCount: 1,
    dimension: '2d',
    format: 'depth24plus-stencil8',
    usage: GPUTextureUsage.OUTPUT_ATTACHMENT | GPUTextureUsage.COPY_SRC
});
let depthTextureView = depthTexture.createView();
```

https://gpuweb.github.io/gpuweb/#dom-gpudevice-createtexture
https://gpuweb.github.io/gpuweb/#dictdef-gputexturedescriptor


Now we will send our data to the GPU memory. We already saw vertices. We are in the 3D world space here, but we have not configured the transformation from 3D to 2D, so we are actually ate the (-1,-1)x(1,1) space. That is why out first vertice is (1,-1,0) - bottom right; our second is (-1,-1) - bottom left; and our last is (0, 1, 0), top vertice.

But we also can specify the color of each vertex. Why, and how the color varies from one vertex to the other we will see later, in the rasterizer pass.

Out last buffer is the index. We specify in the "array of vertices" which form the triangles.

![Vertices](images\Vertices.png)

```
const positionBuffer = createBuffer(new Float32Array([
    1.0, -1.0, 0.0,
    -1.0, -1.0, 0.0,
    0.0,  1.0, 0.0]), GPUBufferUsage.VERTEX);
const colorBuffer = createBuffer(new Float32Array([
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0]), GPUBufferUsage.VERTEX);
const indexBuffer = createBuffer(new Uint16Array([ 0, 1, 2 ]), GPUBufferUsage.INDEX);
```

Now we can create the pipeline.

1 - Primitive Topology
2 - Vertex State
3 - Rasterizer State
4 - Fragment State
5 - Depth-Stencil Test State
6 - Color State

Each of these parts are sub-system of its own, so we gonna need a specific subpart for each.

```
const pipeline = device.createRenderPipeline({
    primitiveTopology: 'triangle-list', // 1
    vertexStage, vertexState, layout,   // 2
    rasterizationState,                 // 3
    fragmentState,                      // 4
    depthStencilState,                  // 5
    colorStates: [ colorState ],        // 6
});
```

https://gpuweb.github.io/gpuweb/#dom-gpudevice-createrenderpipeline


## 1 - Primitive Topology

We specify that we will pass "triangles list". In our case just one triangle;

## 2 - Vertex State

Here we are defining all the information needed for each vertex to render our triangle. First we define that how our positions is structured (1). "float3" means that we are passing three floats as the position. "arrayStride" is how much bytes we need to jump from the first to get the next position, 12 because we packed all positions, so just three floats of four bytes each.

We have the same config for colour (2). The only pertinent difference here is the "shaderLocation". We will understand what this means later.

In the end we we configure our "vertex state" binding everything together.

```
const posVB = {
    attributes: [ { shaderLocation: 0, offset: 0, format: 'float3' } ],
    arrayStride: 4 * 3,
    stepMode: 'vertex'
}; // 1
const colorVB = {
    attributes: [ { shaderLocation: 1, offset: 0, format: 'float3' } ],
    arrayStride: 4 * 3,
    stepMode: 'vertex'
}; // 2
const vertexState = {
    indexFormat: 'uint16',
    vertexBuffers: [ posVB, colorVB ]
};
```

## 2.1 - Vertex Shader

```
const vertModule = device.createShaderModule({ code: await loadSPV(vertspv) });
const vertexStage = {
    module: vertModule,
    entryPoint: 'main' };
```

https://gpuweb.github.io/gpuweb/#dom-gpudevice-createshadermodule
https://gpuweb.github.io/gpuweb/#dictdef-gpushadermoduledescriptor
https://gpuweb.github.io/gpuweb/#typedefdef-gpushadercode


```
layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inColor;
layout (location = 0) out vec3 outColor;

void main()
{
    gl_Position = vec4(inPos.xyz, 1.0);
    outColor = inColor;    
}
```

```
> glslangValidator.exe -V ./shaders/shader.vert -o ./shaders/vert.spv
```

## 3 - Rasterizer State

When rendering volumes, like a cube, we are normally looking directly into a face but we are not viewing the faces in the other direction. Think of a dice. If you are seing face 1, you cannot see face 6. But when rendering the dice we ask teh GPU to render all six faces. And we need to do everything with all the fragments of the face six that it is going to be overwritten by face one.

All of this sounds like a huge waste. And it is. One solution is, from the assumption that we will normally render volumes, we can choose one convention of specifying faces as clockwise, as the clock pointer rotates. See why we chose the vertices index as we did. Not imagine looking at this face from the back. From vertex 0, to 1, to 2 would be in counter-clockwise direction. So with this strange, but simple convention we have a simple way of defining if we are seing a face from the front and from the behind.

Here we are configuring that when clockwise we are from the front (1) and that that we should cull, or just ignore all the "back" faces (2).

```
const rasterizationState = {
    frontFace: 'cw', // 1
    cullMode: 'back' // 2
};
```

## 4 - Fragment State

```
const fragModule = device.createShaderModule({ code: await loadSPV(fragspv) });
const fragmentStage = {
    module: fragModule, 
    entryPoint: 'main'
};
```

```
> glslangValidator.exe -V ./shaders/shader.frag -o ./shaders/frag.spv
```

## 5 - Depth-Stencil Test State

In this state we already have the desired color of the fragment. But we also have its "depth". Here we will configure depth test and/or stencil test and decide if we want to merge this fragment to the final image.

We enable depth test (1) and say that the fragment must be written only if its depth is "less" than the target depth buffer value (2).

Stencil test is used to allow finer control when discarding pixels. Is useful in some rendering techniques. Suppose that you want that the middle pixels of our triangle are not rendered, for example, a circle around the center. You can achieve this using the Stencil Buffer, for example.

```
const depthStencilState = {
    depthWriteEnabled: true, // 1
    depthCompare: 'less', // 2
    format: 'depth24plus-stencil8'
};
```

![Depth/Stencil Tests](images/pipeline.pipeline.dst.png)

## 6 - Color State or Blending

The color state configures how what we are rendering color going to be mixed with what already exist in the image. 

For example, we are rendering a colored triangle in a black background. 

```
Color_Result = ColorSource * colorBlend.srcFactor + ColorDestination * colorBlend.dstFactor;
Alpha_Result = AlphaSource * alphaBlend.srcFactor + AlphaDestination * alphaBlend.dstFactor;
```

Source is what we are drawing and destination is what is already on the image. We don't care for transparency for now. So we will simply use:

```
const colorState = {
    format: 'bgra8unorm',
    alphaBlend: { srcFactor: 'one', dstFactor: 'zero', operation: 'add' },
    colorBlend: { srcFactor: 'one', dstFactor: 'zero', operation: 'add' },
    writeMask: GPUColorWrite.ALL
};
```

![Output Merger - Blend](images/pipeline.om.blend.png.png)