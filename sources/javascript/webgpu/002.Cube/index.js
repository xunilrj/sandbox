import 'babel-polyfill';
import vertspv from './shaders/vert.spv';
import fragspv from './shaders/frag.spv';
import cube from './cube.js';

if(!navigator || !navigator.gpu)
    console.error("No WebGPU. Sorry!");

function toRadians(degrees)
{
    return degrees * (2 * Math.PI) / 360;
}

function vecSub(a, b) { return [a[0] - b[0], a[1] - b[1], a[2] - b[2]]; }
function vecNorm(a)
{ 
    const l = Math.sqrt((a[0]*a[0]) + (a[1]*a[1]) + (a[2]*a[2]));
    return [a[0]/l, a[1]/l, a[2]/l];
}
function vecDot(a,b) { return (a[0]*b[0]) + (a[1]*b[1]) + (a[2]*b[2]); }
function vecCross(a,b) { return [a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]]; }

function mat4Eye()
{
    return Float32Array.from([
        1, 0, 0, 0, 
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1,
    ]);
}

function mat4Mul(A, B)
{
    // var a00 = mat[0], a01 = mat[1], a02 = mat[2], a03 = mat[3];
    // var a10 = mat[4], a11 = mat[5], a12 = mat[6], a13 = mat[7];
    // var a20 = mat[8], a21 = mat[9], a22 = mat[10], a23 = mat[11];
    // var a30 = mat[12], a31 = mat[13], a32 = mat[14], a33 = mat[15];

    // var b00 = mat2[0], b01 = mat2[1], b02 = mat2[2], b03 = mat2[3];
    // var b10 = mat2[4], b11 = mat2[5], b12 = mat2[6], b13 = mat2[7];
    // var b20 = mat2[8], b21 = mat2[9], b22 = mat2[10], b23 = mat2[11];
    // var b30 = mat2[12], b31 = mat2[13], b32 = mat2[14], b33 = mat2[15];

    let dest = new Float32Array(16);
    // dest[0] = b00*a00 + b01*a10 + b02*a20 + b03*a30;
    // dest[1] = b00*a01 + b01*a11 + b02*a21 + b03*a31;
    // dest[2] = b00*a02 + b01*a12 + b02*a22 + b03*a32;
    // dest[3] = b00*a03 + b01*a13 + b02*a23 + b03*a33;
    // dest[4] = b10*a00 + b11*a10 + b12*a20 + b13*a30;
    // dest[5] = b10*a01 + b11*a11 + b12*a21 + b13*a31;
    // dest[6] = b10*a02 + b11*a12 + b12*a22 + b13*a32;
    // dest[7] = b10*a03 + b11*a13 + b12*a23 + b13*a33;
    // dest[8] = b20*a00 + b21*a10 + b22*a20 + b23*a30;
    // dest[9] = b20*a01 + b21*a11 + b22*a21 + b23*a31;
    // dest[10] = b20*a02 + b21*a12 + b22*a22 + b23*a32;
    // dest[11] = b20*a03 + b21*a13 + b22*a23 + b23*a33;
    // dest[12] = b30*a00 + b31*a10 + b32*a20 + b33*a30;
    // dest[13] = b30*a01 + b31*a11 + b32*a21 + b33*a31;
    // dest[14] = b30*a02 + b31*a12 + b32*a22 + b33*a32;
    // dest[15] = b30*a03 + b31*a13 + b32*a23 + b33*a33;
    for (let i = 0; i < 4; i++) {
        for (let j = 0; j < 4; j++) {
            dest[j * 4 + i] = 0;
            for (let k = 0; k < 4; k++)
                dest[j * 4 + i] += A[k * 4 + i] * B[j * 4 + k];
        }
    }
    return dest;
}

function mat4ProjFrustum(R, left, right, bottom, top, near, far)
{
    R[0] = -2*near/(left - right);
    R[1] = 0;
    R[2] = 0;
    R[3] = 0;
    R[4] = 0;
    R[5] = -2*near/(bottom - top);
    R[6] = 0;
    R[7] = 0;
    R[8] = 0;
    R[9] = 0;
    R[10] = -far/(far - near);
    R[11] = -1;
    R[12] = near*(left + right)/(left - right);
    R[13] = near*(bottom + top)/(bottom - top);
    R[14] = -far*near/(far - near);
    R[15] = 0;
    return R;
}

function mat4Proj(mat4, fovy, aspect, near, far)
{
    var top, bottom, left, right;
    top = near * Math.tan(toRadians(fovy)/2);
    bottom = -top;
    right = top * aspect;
    left = -right;
    return mat4ProjFrustum(mat4, left, right, bottom, top, near, far);
}

function mat4LookAt(mat4, eye_x, eye_y, eye_z, center_x, center_y, center_z, up_dx, up_dy, up_dz)
{
    const center = [center_x, center_y, center_z];
    const eye = [eye_x, eye_y, eye_z];
    const up = [up_dx, up_dy, up_dz];

    const n = vecNorm(vecSub(center, eye));
    const u = vecNorm(vecCross(up, n));
    const v = vecNorm(vecCross(n, u));

    var tx = -vecDot(u, eye);
    var ty = -vecDot(v, eye);
    var tz = vecDot(n, eye);

    mat4[0] = u[0];
    mat4[1] = v[0];
    mat4[2] = n[0];
    mat4[3] = 0;

    mat4[4] = u[1];
    mat4[5] = v[1];
    mat4[6] = n[1];
    mat4[7] = 0;

    mat4[8]  = u[2];
    mat4[9]  = v[2];
    mat4[10] = n[2];
    mat4[11] = 0;

    mat4[12] = tx;
    mat4[13] = ty;
    mat4[14] = tz;
    mat4[15] = 1;

    return mat4;
};

function mat4Octave(mat)
{
    let row = [];
    for (let i = 0; i < 4; i++) {
        let cols = []
        for (let j = 0; j < 4; j++) {
            cols.push(mat[j * 4 + i]);
        }
        row.push(`[${cols.join(",")}]`);
    }
    return `[${row.join(";")}]`;
}

function quatEye()
{
    return Float32Array.from([
        0, 0, 0, 1
    ]);
}

function quatRot(Q, axis, angle)
{
    let s = Math.sin(angle / 2);
    Q[0] = axis[0] * s;
    Q[1] = axis[1] * s;
    Q[2] = axis[2] * s;
    Q[3] = Math.cos(angle / 2)
}

function mat4Quat(R, q){
    //0 4 8  12
    //1 5 9  13
    //2 6 10 14
    //3 7 11 15
    let sqw = q[3]*q[3];
    let sqx = q[0]*q[0];
    let sqy = q[1]*q[1];
    let sqz = q[2]*q[2];
    
    let invs = 1 / (sqx + sqy + sqz + sqw)
    R[0] = ( sqx - sqy - sqz + sqw)*invs;
    R[5] = (-sqx + sqy - sqz + sqw)*invs;
    R[10] = (-sqx - sqy + sqz + sqw)*invs;
    
    let tmp1 = q[0]*q[1];
    let tmp2 = q[2]*q[3];
    R[1] = 2.0 * (tmp1 + tmp2)*invs;
    R[4] = 2.0 * (tmp1 - tmp2)*invs;
    
    tmp1 = q[0]*q[2];
    tmp2 = q[1]*q[3];
    R[2] = 2.0 * (tmp1 - tmp2)*invs ;
    R[8] = 2.0 * (tmp1 + tmp2)*invs ;
    
    tmp1 = q[1]*q[2];
    tmp2 = q[0]*q[3];
    R[9] = 2.0 * (tmp1 + tmp2)*invs ;
    R[6] = 2.0 * (tmp1 - tmp2)*invs ; 
    
    R[3] = 0;
    R[7] = 0;
    R[11] = 0;

    R[12] = 0;
    R[13] = 0;
    R[14] = 0;
    R[15] = 1;

    return R;
} 

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

    //Swapchain
    const swapChainFormat = "rgba8unorm";
    const canvas = document.getElementById("screen");
    const ctx = canvas.getContext('gpupresent')
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

    // Shaders
    async function loadSPV(url)
    {
        const res = await fetch(url);
        const buffer = await res.arrayBuffer();
        return new Uint32Array(buffer);
    }
    const vs = device.createShaderModule({ code: await loadSPV(vertspv) });
    const fs = device.createShaderModule({ code: await loadSPV(fragspv) });

    const uniformGroupLayout = device.createBindGroupLayout({
        bindings: [{
            binding: 0,
            visibility: GPUShaderStage.VERTEX,
            type: 'uniform-buffer'
        }]
    });
    const layout = device.createPipelineLayout({ 
        bindGroupLayouts: [uniformGroupLayout] 
    });
    let cubeInstance = cube.create(device, vs, fs, swapChainFormat, layout, {mode:"wireframe"});

    let mangle = 0;
    let mquat = quatEye();
    quatRot(mquat, [0,1,0], mangle);
    let quatMat4 = mat4Eye();    
    
    let pmat4 = mat4Proj(mat4Eye(), 45, canvas.width / canvas.height, 1, 100);
    let vmat4 = mat4LookAt(mat4Eye(), 5, 5, 5, 0, 0, 0, 0, 1, 0);
    let pvm = mat4Mul(pmat4, mat4Mul(vmat4, mat4Quat(quatMat4, mquat)));

    let uniformBuffer = device.createBuffer({
        size: pvm.byteLength,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
    });
    let uniformBindGroup = device.createBindGroup({
        layout: uniformGroupLayout,
        bindings: [{
            binding: 0,
            resource: { buffer: uniformBuffer }
        }]
    });

    function sendCommands(queue, backBufferView, depthTextureView)
    {
        mangle += 0.001;
        quatRot(mquat, [0,1,0], mangle);
        pvm = mat4Mul(pmat4, mat4Mul(vmat4, mat4Quat(quatMat4, mquat)));

        uniformBuffer.setSubData(0, pvm);
        const commands = [
            cubeInstance.render(canvas, device, backBufferView, depthTextureView, uniformBindGroup)
        ];
        queue.submit(commands);
    }

    let render = () => {        
        const backBuffer = swapchain.getCurrentTexture();
        const backBufferView = backBuffer.createView();
    
        sendCommands(device.defaultQueue, backBufferView, depthTextureView);
        requestAnimationFrame(render);
    };
    return render;
}

setup().then(render => {
    requestAnimationFrame(render);
});