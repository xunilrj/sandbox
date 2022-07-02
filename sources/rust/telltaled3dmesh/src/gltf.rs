use json::JsonValue;

pub fn push_buffer<T>(value: &mut JsonValue, buffer: &[T]) -> JsonValue {
    let buffer = unsafe {
        std::slice::from_raw_parts(
            buffer.as_ptr() as *const u8,
            buffer.len() * std::mem::size_of::<T>(),
        )
    };
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

    json::object! {
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

pub fn push_scene(gltf: &mut JsonValue, scene: JsonValue) -> usize {
    if !gltf["scenes"].is_array() {
        gltf["scenes"] = json::array![];
    }

    let _ = gltf["scenes"].push(scene);
    gltf["scenes"].len() - 1
}

pub fn get_scene_0_mut(gltf: &mut JsonValue) -> &mut JsonValue {
    if !gltf["scenes"].is_array() {
        gltf["scenes"] = json::array![];
    }

    if gltf["scenes"].len() == 0 {
        let _ = gltf["scenes"].push(json::object! {
             nodes: []
        });
    }

    &mut gltf["scenes"][0]
}

pub fn push_node(gltf: &mut JsonValue, node: JsonValue) -> usize {
    if !gltf["nodes"].is_array() {
        gltf["nodes"] = json::array![];
    }

    let _ = gltf["nodes"].push(node);
    gltf["nodes"].len() - 1
}

pub fn push_material(gltf: &mut JsonValue, material: JsonValue) -> usize {
    if !gltf["materials"].is_array() {
        gltf["materials"] = json::array![];
    }

    let _ = gltf["materials"].push(material);
    gltf["materials"].len() - 1
}

pub fn push_texture(gltf: &mut JsonValue, texture: JsonValue) -> usize {
    if !gltf["textures"].is_array() {
        gltf["textures"] = json::array![];
    }

    let _ = gltf["textures"].push(texture);
    gltf["textures"].len() - 1
}

pub fn push_image(gltf: &mut JsonValue, image: JsonValue) -> usize {
    if !gltf["images"].is_array() {
        gltf["images"] = json::array![];
    }

    let _ = gltf["images"].push(image);
    gltf["images"].len() - 1
}
