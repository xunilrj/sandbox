use std::path::PathBuf;
use json::JsonValue;

use crate::d3dmesh::d3dfile::D3DFile;

pub fn add_mesh_to_gltf(mesh: &D3DFile, gltf: &mut JsonValue) {
    let indices = mesh.get_buffer("index");
    let indices = indices.as_u16().as_slice();
    let mut newindices = vec![];
    for (i, m) in mesh.meshes.iter().enumerate() {
        let start = m.index_start;
        let end = start + (m.tri_count * 3);
        for fi in start..end {
            newindices.push(indices[fi]);
        }
    }
    let indices = newindices.as_slice();
    let mut indices_buffer_view = crate::gltf::push_buffer(gltf, indices);
    indices_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let indices_buffer_view_idx = crate::gltf::push_buffer_view(gltf, indices_buffer_view);
    let indices_acessor_idx = crate::gltf::push_accessor(gltf, 
        json::object! {
            bufferView: indices_buffer_view_idx,
            componentType: 5123,
            count: indices.len(),
            type: "SCALAR",
            byteOffset: 0,
        }
    );
    

    let vertices = mesh.get_buffer("position");
    let vertices = vertices.as_f32().as_slice();
    let mut vertices_buffer_view = crate::gltf::push_buffer(gltf, vertices);
    vertices_buffer_view["target"] = json::JsonValue::Number(34962i32.into());
    let vertices_buffer_view_idx = crate::gltf::push_buffer_view(gltf, vertices_buffer_view);
    let vertices_acessor_idx = crate::gltf::push_accessor(gltf, 
        json::object! {
            bufferView: vertices_buffer_view_idx,
            componentType: 5126,
            count: vertices.len() / 3,
            type: "VEC3",
            byteOffset: 0,
        }
    );

    let bone_idx = mesh.get_buffer("bone_idx");
    let bone_idx = bone_idx.as_u8().as_slice();
    let mut bone_idx_view = crate::gltf::push_buffer(gltf, bone_idx);
    bone_idx_view["target"] = json::JsonValue::Number(34962i32.into());
    let bone_idx_view_idx = crate::gltf::push_buffer_view(gltf, bone_idx_view);
    let bone_idx_acessor_idx = crate::gltf::push_accessor(gltf, 
        json::object! {
            bufferView: bone_idx_view_idx,
            componentType: 5121,
            count: bone_idx.len() / 4,
            type: "VEC4",
            byteOffset: 0,
        }
    );

    let bone_weights = mesh.get_buffer("bone_weigth");
    let bone_weights = bone_weights.as_f32().as_slice();
    let mut bone_weights_view = crate::gltf::push_buffer(gltf, bone_weights);
    bone_weights_view["target"] = json::JsonValue::Number(34962i32.into());
    let bone_weights_view_idx = crate::gltf::push_buffer_view(gltf, bone_weights_view);
    let bone_weights_acessor_idx = crate::gltf::push_accessor(gltf, 
        json::object! {
            bufferView: bone_weights_view_idx,
            componentType: 5126,
            count: bone_weights.len() / 4,
            type: "VEC4",
            byteOffset: 0,
        }
    );

    crate::gltf::push_mesh(gltf, json::object! {
        primitives : [{
            attributes: {
                POSITION: vertices_acessor_idx,
                JOINTS_0: bone_idx_acessor_idx,
                WEIGHTS_0: bone_weights_acessor_idx
            },
            indices: indices_acessor_idx
        }]
    });
}


pub fn save_to_gltf(mesh: &D3DFile, path: PathBuf) {

}