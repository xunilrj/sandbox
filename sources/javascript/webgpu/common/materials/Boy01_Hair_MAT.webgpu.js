async function createTexture (device, url) {
    const res = await fetch(url);
    const imgBuffer = await res.arrayBuffer();
    
    const size = {
        width: 1024,
        height: 1024,
        depth: 1
    };
    const format = "rgba8unorm";
    const texture = device.createTexture({
        size,
        format,
        usage: GPUTextureUsage.COPY_DST | GPUTextureUsage.SAMPLED
    });
    let rowPitch = 1024 * 4;
    const buffer = device.createBuffer({
        size: imgBuffer.byteLength,
        usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.COPY_SRC
    });
    buffer.setSubData(0, new Uint32Array(imgBuffer));

    const q = device.defaultQueue;
    const f = q.createFence({initialValue: 0});

    const enc = device.createCommandEncoder({});        
    enc.copyBufferToTexture(
        { buffer, rowPitch, imageHeight: 0},
        { texture },
        size
    );        
    q.submit([enc.finish()]);        
    q.signal(f, 1);
    
    await f.onCompletion(1);
    return {texture};
}
async function loadSPV(url)
{
    const res = await fetch(url);
    const buffer = await res.arrayBuffer();
    return new Uint32Array(buffer);
}

export async function load(device)
{
    const vs = device.createShaderModule({ code: await loadSPV("http://localhost:5000/shaders/Boy01_Hair_MAT.vert.spv") });
    const fs = device.createShaderModule({ code: await loadSPV("http://localhost:5000/shaders/Boy01_Hair_MAT.frag.spv") });
    const sampler = device.createSampler({ minFilter: "linear", magFilter: "linear" });
    const tex_CHANNEL0 = await createTexture(device, "http://localhost:5000/textures/Boy_Hair_diffuse");
    return {vs, fs, sampler, textures: [tex_CHANNEL0]};
}

export default { load };
