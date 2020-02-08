import { mat4 } from '../math/index.js';

async function loadSPV(url)
{
    const res = await fetch(url);
    const buffer = await res.arrayBuffer();
    return new Uint32Array(buffer);
}

async function createRenderObject(device, vs, fs, swapChainFormat, layout, options)
{
    if(!vs) vs = device.createShaderModule({ code: await loadSPV("http://localhost:5000/line/vert.spv") });
    if(!fs) fs = device.createShaderModule({ code: await loadSPV("http://localhost:5000/line/frag.spv") });

    options = options || {};
    
    if(!options.color) options.color = [1,1,1];

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

    var points = [
        0, 0, 0,
        0, 0, 1
    ];
    var colors = [
        options.color[0], options.color[1], options.color[2],
        options.color[0], options.color[1], options.color[2]
    ];

    var indices = [0, 1];
    const indicesLength = indices.length;    
    const positionBuffer = createBuffer(new Float32Array(points), GPUBufferUsage.VERTEX);
    const colorBuffer = createBuffer(new Float32Array(colors), GPUBufferUsage.VERTEX);
    const indexBuffer = createBuffer(new Uint16Array(indices), GPUBufferUsage.INDEX);

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

    const primitiveTopology = "line-list";
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
        pass.drawIndexed(indicesLength, 1, 0, 0, 0);
        pass.endPass();
        return commandEncoder.finish()
    }

    return { render };   
}


class DebugLineRenderSystem
{
    constructor()
    {
        this.lines = [];
        this.bindingGroupLayout = null;
        this.renderObject = null;
        this.cameraSystem = null;
    } 

    createBindingGroup(device, arr)
    {
        let buffer = device.createBuffer({ size: arr.byteLength, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
        let bindGroup = device.createBindGroup({ layout: this.bindingGroupLayout, bindings: [{ binding: 0, resource: { buffer } }] });
        return [buffer, bindGroup, arr];
    }
    
    pushLines(device, lines)
    {
        return lines.map(([a,b]) => {
            const [buffer, bindGroup, matrix] = this.createBindingGroup(device, mat4.identity());
            const line = { buffer, bindGroup, matrix, a, b };
            this.lines.push(line);
            return line;
        });
    }

    render(canvas, device, backBufferView, depthTextureView)
    {
        let cam = this.cameraSystem.getActiveCamera();
        let pv = cam.update();
        return this.lines.flatMap((x,i) => {
            let va = mat4.mulVec4(pv, x.a);
            let vb = mat4.mulVec4(pv, x.b);

            x.matrix[0] = va[0]; x.matrix[1] = va[1]; x.matrix[2] = va[2]; x.matrix[3] = va[3];
            x.matrix[4] = vb[0]; x.matrix[5] = vb[1]; x.matrix[6] = vb[2]; x.matrix[7] = vb[3];

            x.buffer.setSubData(0, x.matrix);
            return [this.renderObject.render(canvas, device, backBufferView, depthTextureView, x.bindGroup)];
        });
    }
}

export default {createRenderObject, DebugLineRenderSystem};