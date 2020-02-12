import 'babel-polyfill';
import vertspv from './shaders/vert.spv';
import fragspv from './shaders/frag.spv';
import Boy01_Head_GeoMesh from '../common/models/Boy01_Head_GeoMesh.webgpu.js';
import Boy01_Head_MAT from '../common/materials/Boy01_Head_MAT.webgpu.js';
import cube from '../common/cube.js';
import circle from '../common/circle.js';
import line from '../common/line/index.js';
import {vec3, vec4, mat4, quat, distances} from '../common/math/index.js';
import {Camera, CameraDebugRenderSystem} from '../common/camera/index.js';
import handleMouse from '../common/mouse.js';
import parse from 'minimist';

if(!navigator || !navigator.gpu)
    console.error("No WebGPU. Sorry!");

let canvas;
let cameraDebugRenderSystem = new CameraDebugRenderSystem();
let lineDebugRenderSystem = new line.DebugLineRenderSystem();
lineDebugRenderSystem.cameraSystem = cameraDebugRenderSystem;
let systems = [
    cameraDebugRenderSystem,
    lineDebugRenderSystem
];

class GameConsole
{
    constructor()
    {
        this.commands = {};
    }

    addCommand(name, f)
    {
        this.commands[name] = f;
    }

    run(cmd)
    {
        let o = parse(cmd.split(" "));
        Object.keys(o).forEach(x => { try { o[x] = eval(o[x]); } catch { } });
        console.log(o);
        let aggcmd = o._.join(".");
        if(this.commands[aggcmd])
        {
            this.commands[aggcmd].call(this, o);
        }
    }
}

window.gameConsole = new GameConsole();
window.gameConsole.addCommand("camera.add", function(obj)
{
    let params = Object.assign({
        fovy: 45, 
        aspect: this.services.canvas.width / this.services.canvas.height, 
        near: 1, 
        far: 10,
        position: [2, 2, 2],
        target: [0, 0, 0]
    }, obj);
    cameraDebugRenderSystem.pushCamera(this.services.device, Camera.fromTo(
        params,
        params,
    ), lineDebugRenderSystem)
});
window.gameConsole.addCommand("camera.active", function(obj)
{
    cameraDebugRenderSystem.setActiveCamera(obj.index)
});

async function setupWebGPU(id)
{
    // Check webGPU is supported
    if(!navigator.gpu) {
        console.error("webGPU not supported")
        throw "webGPU not supported";
    }

    const gpu = navigator.gpu;
    const adapter = await gpu.requestAdapter();
    const device = await adapter.requestDevice();

    const canvas = document.getElementById(id);
    const ctx = canvas.getContext('gpupresent');

    window.gameConsole.services = {
        canvas: canvas,
        device: device
    };

    return {gpu, adapter, device, canvas, ctx};
}

async function setupRenderTargets(device, canvas, ctx, swapChainFormat)
{
    const swapchain = ctx.configureSwapChain({
        device: device,
        format: swapChainFormat
    });
    const depthTexture = device.createTexture({
        size: {
            width: canvas.width,
            height: canvas.height,
            depth: 1
        },
        arrayLayerCount: 1,
        mipLevelCount: 1,
        sampleCount: 1,
        dimension: '2d',
        format: 'depth32float',
        usage: GPUTextureUsage.OUTPUT_ATTACHMENT | GPUTextureUsage.COPY_SRC
    });
    const depthTextureView = depthTexture.createView();

    return {swapchain, depthTexture, depthTextureView};
}

async function setup()
{
    const canvasId = "screen";
    const swapChainFormat = "rgba8unorm";

    let {gpu, adapter, device, canvas, ctx} =
        await setupWebGPU(canvasId);
    
    let {swapchain, depthTexture, depthTextureView} =
        await setupRenderTargets(device, canvas, ctx, swapChainFormat);
    
    let createBindGroupLayout = (device, bindings) => {
        let bs = bindings.map((x,i) => ({ binding: i, type: x.type, visibility: x.visibility}));
        const bindGroupLayouts = [device.createBindGroupLayout({bindings: bs})];
        const layout = device.createPipelineLayout({ bindGroupLayouts });

        let newBindGroup = (args) => {
            let buffers = bindings.map((x,i) => {
                if(x.type != 'uniform-buffer') return args[i];

                let value = args[i];
                let buffer = device.createBuffer({
                    size: value.byteLength,
                    usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
                })
                buffer.setSubData(0, value);
                return { buffer };
            });

            let bindGroup = device.createBindGroup({
                layout: bindGroupLayouts[0],
                bindings: bindings.map((x,i) => (
                    { binding: i, resource: buffers[i] }
                ))
            });

            return { bindGroup, args, buffers: buffers.map(x => {
                if(x.buffer) return x.buffer;
                else return x;
            })};
        }

        return {bindGroupLayout: bindGroupLayouts[0], pipelineLayout: layout, newBindGroup};
    }

    let {bindGroupLayout, pipelineLayout, newBindGroup} = createBindGroupLayout(device,[
        { type: 'uniform-buffer', visibility: GPUShaderStage.VERTEX},
        { type: 'sampler', visibility: GPUShaderStage.FRAGMENT },
        { type: "sampled-texture", visibility: GPUShaderStage.FRAGMENT }
    ]);
    

    let headMaterial = await Boy01_Head_MAT.load(device);
    let instance = await Boy01_Head_GeoMesh.load(device, headMaterial.vs, headMaterial.fs, swapChainFormat, pipelineLayout, 
        "http://localhost:5000/models/Boy01_Head_GeoMesh.buffer",
        "http://localhost:5000/models/Boy01_Head_GeoMesh.index");

    let g1Quat = quat.identity();
    let g1ModelMatrix = mat4.identity();
    let g1 = newBindGroup([
        mat4.identity(),
        headMaterial.sampler,
        headMaterial.textures[0].texture.createView()
    ]);
    
    const cubeBindGroupLayout = device.createBindGroupLayout({
        bindings: [{binding: 0, visibility: GPUShaderStage.VERTEX, type: 'uniform-buffer' }]
    });
    const cubePipelineLayout = device.createPipelineLayout({ bindGroupLayouts:[cubeBindGroupLayout] });
    let cubeInstance = await cube.create(device, null, null, swapChainFormat, cubePipelineLayout, {
        mode:"wireframe",
        scale: instance.boundingBox.scale
    });

    // config camera debug render
    cameraDebugRenderSystem.cameraBindingGroupLayout = cubeBindGroupLayout;
    cameraDebugRenderSystem.cameraRenderObject = cubeInstance;

    

    // let rotyInstance = await circle.create(device, null, null, swapChainFormat, pipelineLayout);
    // let rotyGrabber = await circle.create(device, null, null, swapChainFormat, pipelineLayout, {
    //     center: [0, 0, 1],
    //     radius: 0.1
    // });

    lineDebugRenderSystem.bindingGroupLayout = cubeBindGroupLayout;
    lineDebugRenderSystem.renderObject = await line.createRenderObject(device, null, null, swapChainFormat, cubePipelineLayout);
        
    cameraDebugRenderSystem.pushCamera(device, Camera.fromTo(
        { fovy: 45, aspect: canvas.width / canvas.height, near: 1, far: 1000 },
        { position: [4, 4, 4], target: [0, 0, 0] }
    ));
    cameraDebugRenderSystem.setActiveCamera(0);

    // let [rayUniformBuffer, rayUniformBindGroup, rayPos] = createUniform(uniformGroupLayout, mat4.identity());
    
    // CAMERAS

    // TODO: load camera from COLLADA
    // TODO: save camera changes TO the COLLADA
    // TODO: render a more beautiful camera

    // TODO: Selectables: rotate head
    // TODO: Nothing selected we still move the camera
    // TODO: mouse click params: buttons and modifiers (control, alt etc...)        
    
    // TODO: convert math to webassembly
    
    // TODO: shader file manager
    // TODO: shader file reload (dynamic config pipeline)
    // TODO: PASS DSL

    let selectable = [
        {center: [0,0,1], radius: 0.05}
    ]
    let raynear = vec4.zero(), rn = vec4.zero(), rayfar = vec4.zero(), rf = vec4.zero();
    let theta = 3, phi = 2;
    let mouse = handleMouse(canvas, {
        move: ({current}) => {
            const cam = cameraDebugRenderSystem.getActiveCamera();
            const b = cam.update();
            const {near, far, dir } = vec3.unproject(b, canvas, current)

            for(var i = 0;i< selectable.length; ++i)
            {
                let c = selectable[i].center;
                let d = distances.lineSegment3Sphere(near, dir, c);
                if(d < selectable[i].radius)
                    return {grabbable: true}
            }

            return null;
        },
        drag: ({delta}) => {
            theta += 5 * delta.dx / canvas.width;
            phi -= 5 * delta.dy / canvas.height;
        }
    });
    
    let [pvmUniformBuffer, pvmUniformBindGroup, pvm] = cameraDebugRenderSystem.createCameraBindingGroup(device, mat4.identity());
    function sendCommands(queue, backBufferView)
    {
        function send(obj, uniformBindGroup)
        {
            if(obj.render)
                return obj.render(canvas, device, backBufferView, depthTextureView, uniformBindGroup)
        }

        let cam = cameraDebugRenderSystem.cameras[0].camera;
        // cam.view.pos[0] = Math.sin(theta) * 15;
        // cam.view.pos[1] = Math.cos(phi) * 15;
        // cam.view.pos[2] = Math.cos(theta) * Math.sin(phi) * 15;

        cam = cameraDebugRenderSystem.getActiveCamera();
        mat4.fromQuat(g1ModelMatrix, g1Quat);
        mat4.identity(g1.args[0]);
        mat4.mul(g1.args[0], cam.update(), g1ModelMatrix);
        // pvmUniformBuffer.setSubData(0, pvm);
        g1.buffers[0].setSubData(0, g1.args[0]);

        pvmUniformBuffer.setSubData(0, g1.args[0]);
        const commands = [
            send(instance, g1.bindGroup),
            send(cubeInstance, pvmUniformBindGroup),
            // send(rotyInstance, pvmUniformBindGroup),
            // send(rotyGrabber, pvmUniformBindGroup),
        ];
        queue.submit(commands);

        systems.forEach(x => {
            if(x.render)
            {
                let commands = x.render(canvas, device, backBufferView, depthTextureView);
                if(commands && commands.length)
                    queue.submit(commands);
            }
        })
        // if(raynear)
        // {
        //     var b = cam.update();

        //     mat4.mulVec4(b, raynear, rn);
        //     mat4.mulVec4(b, rayfar, rf);

        //     rayPos[0] = rn[0];
        //     rayPos[1] = rn[1];
        //     rayPos[2] = rn[2];
        //     rayPos[3] = rn[3];
           
        //     rayPos[4] = rf[0];
        //     rayPos[5] = rf[1];
        //     rayPos[6] = rf[2];
        //     rayPos[7] = rf[3];
        //     rayUniformBuffer.setSubData(0, rayPos);
        //     commands.push(send(mouseRay, rayUniformBindGroup));
        // }
    }

    let render = (timestamp) => {        
        mouse.currentTime = timestamp;
        const colorTexture = swapchain.getCurrentTexture();
        const backBufferView = colorTexture.createView();

        sendCommands(device.defaultQueue, backBufferView);
        requestAnimationFrame(render);
    };
    return render;
}

setup().then(render => {
    requestAnimationFrame(render);
});