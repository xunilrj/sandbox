async function loadBuffer(url)
{
    const res = await fetch(url);
    return await res.arrayBuffer();
}

function createBuffer(device, from, usage)
 {
    let [buffer, to] = device.createBufferMapped({ size: from.byteLength, usage });
    new Uint8Array(to).set(new Uint8Array(from));
    buffer.unmap();
    return buffer;
};

export async function load(device, vs, fs, swapChainFormat, layout, vbFile, ibFile)
{
    let vb = createBuffer(device, await loadBuffer(vbFile || 'Boy01_UpperBody_GeoMesh.buffer'), GPUBufferUsage.VERTEX );
    let ib = createBuffer(device, await loadBuffer(ibFile || 'Boy01_UpperBody_GeoMesh.index'), GPUBufferUsage.INDEX );
    const vertexState = { indexFormat: 'uint16', vertexBuffers: [{
        attributes: [
            { shaderLocation: 0, offset: 0, format: 'float3' }, //VERTEX
            { shaderLocation: 1, offset: 12, format: 'float3' }, //NORMAL
            { shaderLocation: 2, offset: 24, format: 'float2' }, //TEXCOORD
        ], arrayStride: 32
    }]};
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
    const pipeline = device.createRenderPipeline({
        primitiveTopology: 'triangle-list', vertexState,
        vertexStage, layout,
        rasterizationState,
        fragmentStage,
        colorStates: [ colorState ],
        depthStencilState
    });

    function render(canvas, device, backBufferView, depthTextureView, uniformBindGroups)
    {
        let colorAttachment = {
            attachment: backBufferView,
            loadValue: { r: 0, g: 0, b: 0, a: 1 },
            storeOp: 'store'
        };
    
        const depthAttachment = {
            attachment: depthTextureView,
            depthLoadValue: 1,            
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
        if(uniformBindGroups.length) {
            for(var i = 0;i < uniformBindGroups.length; ++i) pass.setBindGroup(i, uniformBindGroups[i]);
        } else {
            pass.setBindGroup(0, uniformBindGroups);
        }
        pass.setVertexBuffer(0, vb);
        pass.setIndexBuffer(ib);
        pass.drawIndexed(4140, 1, 0, 0, 0);
        pass.endPass();
        return commandEncoder.finish()
    }
    let boundingBox = {min:[-55.304751,78.345066,-11.123484],max:[55.304751,139.693682,12.760334],scale:[1,0.5546414629007189,0.21592917035283277]};
    return { render, boundingBox };
}
export default { load };
