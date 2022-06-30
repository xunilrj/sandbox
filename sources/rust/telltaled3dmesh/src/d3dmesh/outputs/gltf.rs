use json::JsonValue;
use std::path::PathBuf;

use crate::d3dmesh::d3dfile::D3DFile;

pub fn empty_gltf() -> json::JsonValue {
    let asset = json::object! {
        generator: "telltale-converter",
        version: "2.0"
    };

    let mut gltf = json::object! {
        asset: asset,
    };

    gltf
}

pub fn add_mesh_to_gltf(mesh: &D3DFile, gltf: &mut JsonValue) {
    let indices = mesh.get_buffer("index").unwrap();
    let indices = indices.as_u16().as_slice();
    let mut newindices = vec![];

    for (_, m) in mesh.meshes.iter().take(9999).enumerate() {
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
    let indices_acessor_idx = crate::gltf::push_accessor(
        gltf,
        json::object! {
            bufferView: indices_buffer_view_idx,
            componentType: 5123,
            count: indices.len(),
            type: "SCALAR",
            byteOffset: 0,
        },
    );

    let mut primitive = json::object! {
        attributes: {},
        indices: indices_acessor_idx
    };

    if let Some(vertices) = mesh.get_buffer("position") {
        let vertices = vertices.as_f32().as_slice();
        let mut vertices_buffer_view = crate::gltf::push_buffer(gltf, vertices);
        vertices_buffer_view["target"] = json::JsonValue::Number(34962i32.into());
        let vertices_buffer_view_idx = crate::gltf::push_buffer_view(gltf, vertices_buffer_view);
        let vertices_acessor_idx = crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: vertices_buffer_view_idx,
                componentType: 5126,
                count: vertices.len() / 3,
                type: "VEC3",
                byteOffset: 0,
            },
        );
        primitive["attributes"].insert("POSITION", JsonValue::Number(vertices_acessor_idx.into()));
    }

    if let Some(uvs) = mesh.nth_buffer(0, "3") {
        let uvs = uvs.as_f32().as_slice();
        let mut uvs_buffer_view = crate::gltf::push_buffer(gltf, uvs);
        uvs_buffer_view["target"] = json::JsonValue::Number(34962i32.into());
        let uvs_buffer_view_idx = crate::gltf::push_buffer_view(gltf, uvs_buffer_view);
        let uvs_acessor_idx = crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: uvs_buffer_view_idx,
                componentType: 5126,
                count: uvs.len() / 2,
                type: "VEC2",
                byteOffset: 0,
            },
        );
        primitive["attributes"].insert("TEXCOORD_0", JsonValue::Number(uvs_acessor_idx.into()));
        primitive["material"] = json::JsonValue::Number(0.into());
    }

    if let Some(bone_idx) = mesh.get_buffer("bone_idx") {
        let bone_idx = bone_idx.as_u8().as_slice();
        let mut bone_idx_view = crate::gltf::push_buffer(gltf, bone_idx);
        bone_idx_view["target"] = json::JsonValue::Number(34962i32.into());
        let bone_idx_view_idx = crate::gltf::push_buffer_view(gltf, bone_idx_view);
        let bone_idx_acessor_idx = crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: bone_idx_view_idx,
                componentType: 5121,
                count: bone_idx.len() / 4,
                type: "VEC4",
                byteOffset: 0,
            },
        );
        primitive["attributes"].insert("JOINTS_0", JsonValue::Number(bone_idx_acessor_idx.into()));
    }

    if let Some(bone_weights) = mesh.get_buffer("bone_weigth") {
        let bone_weights = bone_weights.as_f32().as_slice();
        let mut bone_weights_view = crate::gltf::push_buffer(gltf, bone_weights);
        bone_weights_view["target"] = json::JsonValue::Number(34962i32.into());
        let bone_weights_view_idx = crate::gltf::push_buffer_view(gltf, bone_weights_view);
        let bone_weights_acessor_idx = crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: bone_weights_view_idx,
                componentType: 5126,
                count: bone_weights.len() / 4,
                type: "VEC4",
                byteOffset: 0,
            },
        );
        primitive["attributes"].insert(
            "WEIGHTS_0",
            JsonValue::Number(bone_weights_acessor_idx.into()),
        );
    }

    crate::gltf::push_mesh(
        gltf,
        json::object! {
            primitives : [primitive]
        },
    );

    crate::gltf::push_scene(
        gltf,
        json::object! {
             nodes: [0]
        },
    );

    crate::gltf::push_node(
        gltf,
        json::object! {
            mesh: 0,
        },
    );
}

pub fn save_to_gltf(mesh: &D3DFile, output: PathBuf) {
    let mut gltf = empty_gltf();
    add_mesh_to_gltf(mesh, &mut gltf);

    crate::skl::gltf::save(output, &gltf);
}
