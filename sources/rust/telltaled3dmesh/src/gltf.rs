use json::JsonValue;

pub fn push_buffer<T>(value: &mut JsonValue, buffer: &[T]) -> JsonValue {
    let buffer = unsafe { std::slice::from_raw_parts(
        buffer.as_ptr() as *const u8, 
        buffer.len() * std::mem::size_of::<T>()
    ) };
    let byte_length = buffer.len();
    let buffer = base64::encode(buffer);
    let buffer = json::object! {
        uri: format!("data:application/gltf-buffer;base64,{}", buffer),
        byteLength: byte_length
    };

    if !value["buffers"].is_array() {
        value["buffers"] = json::array![];
    }

    let _ = value["buffers"].push(buffer);

    json::object!{
        buffer: value["buffers"].len() - 1,
        byteLength: byte_length,
    }
}

pub fn push_buffer_view(gltf: &mut JsonValue, buffer_view: JsonValue) -> usize {
    if !gltf["bufferViews"].is_array() {
        gltf["bufferViews"] = json::array![];
    }

    let _ = gltf["bufferViews"].push(buffer_view);
    gltf["bufferViews"].len() - 1
}

pub fn push_accessor(gltf: &mut JsonValue, acessor: JsonValue) -> usize {
    if !gltf["accessors"].is_array() {
        gltf["accessors"] = json::array![];
    }

    let _ = gltf["accessors"].push(acessor);
    gltf["accessors"].len() - 1
}

pub fn push_sampler(anim: &mut JsonValue, sampler: JsonValue) -> usize {
    if !anim["samplers"].is_array() {
        anim["samplers"] = json::array![];
    }

    let _ = anim["samplers"].push(sampler);
    anim["samplers"].len() - 1
}

pub fn push_channel(anim: &mut JsonValue, channels: JsonValue) -> usize {
    if !anim["channels"].is_array() {
        anim["channels"] = json::array![];
    }

    let _ = anim["channels"].push(channels);
    anim["channels"].len() - 1
}

pub fn push_mesh(gltf: &mut JsonValue, mesh: JsonValue) -> usize {
    if !gltf["meshes"].is_array() {
        gltf["meshes"] = json::array![];
    }

    let _ = gltf["meshes"].push(mesh);
    gltf["meshes"].len() - 1
}
