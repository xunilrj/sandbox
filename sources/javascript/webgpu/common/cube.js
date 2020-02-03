
 async function loadSPV(url)
 {
     const res = await fetch(url);
     const buffer = await res.arrayBuffer();
     return new Uint32Array(buffer);
 }

async function create(device, vs, fs, swapChainFormat, layout, options)
{
    if(!vs) vs = device.createShaderModule({ code: await loadSPV("http://localhost:5000/wireframe.vert.spv") });
    if(!fs) fs = device.createShaderModule({ code: await loadSPV("http://localhost:5000/wireframe.frag.spv") });

    options = options || {
        mode: "solid",
        normal: false
    };
    if(!options.scale) options.scale = [1,1,1];
    let createBuffer = (arr, usage) => {
        let desc = { size: arr.byteLength, usage };
        let [ buffer, bufferMapped ] = device.createBufferMapped(desc);
        const writeArray =
            arr instanceof Uint16Array ? 
            new Uint16Array(bufferMapped) :
            new Float32Array(bufferMapped);
        writeArray.set(arr);
        buffer.unmap();
        return buffer;
    };
    
    const positionBuffer = createBuffer(new Float32Array([
        options.scale[0] * -1.0, options.scale[1] * -1.0, options.scale[2] *  1.0,
        options.scale[0] *  1.0, options.scale[1] * -1.0, options.scale[2] *  1.0,
        options.scale[0] *  1.0, options.scale[1] *  1.0, options.scale[2] *  1.0,
        options.scale[0] * -1.0, options.scale[1] *  1.0, options.scale[2] *  1.0,
        options.scale[0] * -1.0, options.scale[1] * -1.0, options.scale[2] * -1.0,
        options.scale[0] *  1.0, options.scale[1] * -1.0, options.scale[2] * -1.0,
        options.scale[0] *  1.0, options.scale[1] *  1.0, options.scale[2] * -1.0,
        options.scale[0] * -1.0, options.scale[1] *  1.0, options.scale[2] * -1.0
    ]), GPUBufferUsage.VERTEX);

    let colorBuffer;
    let indexBuffer;
    if(options.mode == "solid")
    {
        colorBuffer = createBuffer(new Float32Array([
            // front colors
            0.0, 0.0, 1.0,
            1.0, 0.0, 1.0,
            1.0, 1.0, 1.0,
            0.0, 1.0, 1.0,
            // back colors
            0.0, 0.0, 0.0,
            1.0, 0.0, 0.0,
            1.0, 1.0, 0.0,
            0.0, 1.0, 0.0
        ]), GPUBufferUsage.VERTEX);
        indexBuffer = createBuffer(new Uint16Array([ 
        // front
		0, 1, 2,
		2, 3, 0,
		// right
		1, 5, 6,
		6, 2, 1,
		// back
		7, 6, 5,
		5, 4, 7,
		// left
		4, 0, 3,
		3, 7, 4,
		// bottom
		4, 5, 1,
		1, 0, 4,
		// top
		3, 2, 6,
		6, 7, 3
        ]), GPUBufferUsage.INDEX);
    } 
    else if(options.mode == "wireframe") 
    {
        if(!options.color) options.color = [1,1,1];
        let arr = new Float32Array(24);
        for(var i = 0; i < 8; ++i)
        {
            arr[i*3 + 0] = options.color[0];
            arr[i*3 + 1] = options.color[1];
            arr[i*3 + 2] = options.color[2];
        }

        colorBuffer = createBuffer(arr, GPUBufferUsage.VERTEX);
        indexBuffer = createBuffer(new Uint16Array([ 
            // front
            0, 1, 1, 2,
            2, 3, 3, 0,
            // right
            1, 5, 5, 6,
            6, 2, 2, 1,
            // back
            7, 6, 6, 5,
            5, 4, 4, 7,
            // left
            4, 0, 0, 3,
            3, 7, 7, 4,
            // bottom
            4, 5, 5, 1,
            1, 0, 0, 4,
            // top
            3, 2, 2, 6,
            6, 7, 7, 3
        ]), GPUBufferUsage.INDEX);
    }

    // Pipeline
    const positionAttribDesc = { shaderLocation: 0, offset: 0, format: 'float3' };
    const colorAttribDesc = { shaderLocation: 1, offset: 0, format: 'float3' };
    const positionBufferDesc = { attributes: [ positionAttribDesc ], arrayStride: 4 * 3 };
    const colorBufferDesc = { attributes: [ colorAttribDesc ], arrayStride: 4 * 3 };
    const vertexState = { indexFormat: 'uint16', vertexBuffers: [ positionBufferDesc, colorBufferDesc ] };
    const vertexStage = { module: vs, entryPoint: 'main' };
    const fragmentStage = { module: fs, entryPoint: 'main' };
    const depthStencilState = {
        depthWriteEnabled: true,
        depthCompare: 'less',
        format: 'depth32float'
    };
    const colorState = {
        format: swapChainFormat,
        alphaBlend: { srcFactor: 'one', dstFactor: 'zero', operation: 'add' },
        colorBlend: { srcFactor: 'one', dstFactor: 'zero', operation: 'add' },
        writeMask: GPUColorWrite.ALL
    };
    const rasterizationState = {
        frontFace: 'cw',
        cullMode: 'none'
    };

    let primitiveTopology = "triangle-list";
    if(options.mode == "wireframe")
    {
        primitiveTopology = "line-list";
    }
    const pipeline = device.createRenderPipeline({
        primitiveTopology, vertexState,
        vertexStage, layout,
        rasterizationState,
        fragmentStage,
        colorStates: [ colorState ],
        depthStencilState
    });
    
    const render = (canvas, device, backBufferView, depthTextureView, uniformBindGroup) =>
    {
        let colorAttachment = {
            attachment: backBufferView,
            loadValue: "load",
            storeOp: 'store'
        };

        const depthAttachment = {
            attachment: depthTextureView,
            depthLoadValue: "load",            
            depthStoreOp: 'store',
            stencilLoadValue: 'load',
            stencilStoreOp: 'store'
        };

        const renderPassDesc = {
            colorAttachments: [ colorAttachment ],
            depthStencilAttachment: depthAttachment
        };

        let commandEncoder = device.createCommandEncoder()
        let pass = commandEncoder.beginRenderPass(renderPassDesc);
        pass.setPipeline(pipeline);
        pass.setViewport(0, 0, canvas.width, canvas.height, 0, 1);
        pass.setScissorRect(0, 0, canvas.width, canvas.height);
        pass.setBindGroup(0, uniformBindGroup);
        pass.setVertexBuffer(0, positionBuffer);
        pass.setVertexBuffer(1, colorBuffer);
        pass.setIndexBuffer(indexBuffer);
        pass.drawIndexed(36, 1, 0, 0, 0);
        pass.endPass();
        return commandEncoder.finish()
    }

    return { render };   
}

export default { create };