use json::JsonValue;
use log::warn;
use std::{collections::HashMap, path::PathBuf};

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

/// Appends a mesh to an existing gltf
pub fn add_mesh_to_gltf(
    mesh: &D3DFile,
    gltf: &mut JsonValue,
    skip_textures: bool,
    node_idx: Option<usize>,
) {
    // Append at scene 0
    let _ = crate::gltf::get_scene_0_mut(gltf);

    // Push index buffer as is
    let indices = mesh.get_buffer("index").unwrap();
    let buffer = indices.as_u16().as_slice();
    let mut indices_buffer_view = crate::gltf::push_buffer(gltf, buffer);
    indices_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let indices_buffer_view_idx = crate::gltf::push_buffer_view(gltf, indices_buffer_view);

    // Check mesh has position buffer and pushes it.
    let pos_acessor = if let Some(vertices) = mesh.get_buffer("position") {
        let vertices = vertices.as_f32().as_slice();
        let mut vertices_buffer_view = crate::gltf::push_buffer(gltf, vertices);
        vertices_buffer_view["target"] = json::JsonValue::Number(34962i32.into());
        let vertices_buffer_view_idx = crate::gltf::push_buffer_view(gltf, vertices_buffer_view);
        Some(crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: vertices_buffer_view_idx,
                componentType: 5126,
                count: vertices.len() / 3,
                type: "VEC3",
                byteOffset: 0,
            },
        ))
    } else {
        None
    };

    // Check mesh has bone index buffer and pushes it.
    // bones as u8, limited to 256. Fine for now.
    let boneidx_acessor = if let Some(bone_idx) = mesh.get_buffer("bone_idx") {
        if mesh.skl.is_none() {
            warn!("JOINTS_0 without SKL. Probably not what you want. Use the --skl argument to fix these indices");
        }

        let bone_idx = bone_idx.as_u8().as_slice();
        // println!("{:?}", bone_idx);

        let mut bone_idx_view = crate::gltf::push_buffer(gltf, bone_idx);
        bone_idx_view["target"] = json::JsonValue::Number(34962i32.into()); // https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#_bufferview_target
        let bone_idx_view_idx = crate::gltf::push_buffer_view(gltf, bone_idx_view);
        Some(crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: bone_idx_view_idx,
                componentType: 5121, //https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#accessor-data-types
                count: bone_idx.len() / 4,
                type: "VEC4",
                byteOffset: 0,
            },
        ))
    } else {
        None
    };

    // Check mesh has bone weight
    let boneweight_acessor = if let Some(bone_weights) = mesh.get_buffer("bone_weigth") {
        let bone_weights = bone_weights.as_f32().as_slice();

        // println!("{:?}", bone_weights);

        let mut bone_weights_view = crate::gltf::push_buffer(gltf, bone_weights);
        bone_weights_view["target"] = json::JsonValue::Number(34962i32.into());
        let bone_weights_view_idx = crate::gltf::push_buffer_view(gltf, bone_weights_view);
        Some(crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: bone_weights_view_idx,
                componentType: 5126,
                count: bone_weights.len() / 4,
                type: "VEC4",
                byteOffset: 0,
            },
        ))
    } else {
        None
    };

    // Push texture coordinates, if they exist
    let mut uvs_acessors = vec![];
    uvs_acessors.extend(push_texture_coordinate(mesh, 0, gltf));
    uvs_acessors.extend(push_texture_coordinate(mesh, 1, gltf));

    let mut image_idxs = HashMap::new();
    let mut textures_idxs = HashMap::new();

    let sampler_idx = crate::gltf::push_sampler(
        gltf,
        json::object! {
            magFilter: 9729,
            minFilter: 9987,
            wrapS: 33648,
            wrapT: 33648
        },
    );

    // Push empty mesh
    let mesh_idx = crate::gltf::push_mesh(
        gltf,
        json::object! {
            primitives : []
        },
    );

    // Push node
    let node_idx = if let Some(node_idx) = node_idx {
        crate::gltf::merge_node(
            gltf,
            node_idx,
            json::object! {
                mesh: mesh_idx,
            },
        );
        node_idx
    } else {
        crate::gltf::push_node(
            gltf,
            json::object! {
                mesh: mesh_idx,
            },
        )
    };

    // Insert node at scene 0
    let scene = crate::gltf::get_scene_0_mut(gltf);
    if !scene["nodes"].contains(node_idx) {
        let _ = scene["nodes"].push(node_idx);
    }

    // Each mesh in the file will generate
    // a primitive
    for (_, m) in mesh.meshes.iter().enumerate() {
        // First and last triangle of this mesh
        let start = m.index_start;
        let end = start + (m.tri_count * 3);

        // Create index acessor for these triangles
        let indices_acessor_idx = crate::gltf::push_accessor(
            gltf,
            json::object! {
                bufferView: indices_buffer_view_idx,
                byteOffset: start * 2,
                componentType: 5123,
                count: end - start,
                type: "SCALAR",
            },
        );

        // Push primitive for these triangles
        // only with position for now
        let _ = gltf["meshes"][mesh_idx]["primitives"].push(json::object! {
            attributes: {
                POSITION: pos_acessor
            },
            indices: indices_acessor_idx
        });
        let primitive_id = gltf["meshes"][mesh_idx]["primitives"].len() - 1;

        // Push a generic material
        let material_idx = crate::gltf::push_material(
            gltf,
            json::object! {
                pbrMetallicRoughness: {
                    metallicFactor: 0.0,
                    roughnessFactor: 1.0,
                }
            },
        );

        // For each texture of this mesh
        for (i, uv) in uvs_acessors.iter().enumerate() {
            let map = match m.maps.get(i) {
                Some(map) => map,
                None => continue,
            };

            // Set its texture coordinate
            let texcoordi = format!("TEXCOORD_{i}");
            if !skip_textures {
                gltf["meshes"][mesh_idx]["primitives"][primitive_id]["attributes"][texcoordi] =
                    JsonValue::Number((*uv).into());
            }

            // and its texture map name
            let image_idx = if let Some(image_idx) = image_idxs.get(&map.name) {
                *image_idx
            } else {
                let image_idx = crate::gltf::push_image(
                    gltf,
                    json::object! {
                        uri: map.name.as_str()
                    },
                );
                image_idxs.insert(map.name.clone(), image_idx);
                image_idx
            };

            let texture_idx =
                if let Some(texture_idx) = textures_idxs.get(&(sampler_idx, image_idx)) {
                    *texture_idx
                } else {
                    let texture_idx = crate::gltf::push_texture(
                        gltf,
                        json::object! {
                            sampler: sampler_idx,
                            source: image_idx
                        },
                    );
                    textures_idxs.insert((sampler_idx, image_idx), texture_idx);
                    texture_idx
                };

            if !skip_textures {
                match map.r#type.as_str() {
                    "albedo" => {
                        gltf["materials"][material_idx]["pbrMetallicRoughness"]
                            ["baseColorTexture"] = json::object! {
                            index: texture_idx,
                            texCoord: i
                        };
                    }
                    "occlusion" => {
                        gltf["materials"][material_idx]["occlusionTexture"] = json::object! {
                            index: texture_idx,
                            texCoord: i
                        };
                    }
                    _ => {}
                }
            }
        }

        gltf["meshes"][mesh_idx]["primitives"][primitive_id]["material"] =
            JsonValue::Number(material_idx.into());

        // Check primitive has bone information
        if let Some(idx) = boneidx_acessor.as_ref() {
            let _ = gltf["meshes"][mesh_idx]["primitives"][primitive_id]["attributes"]
                .insert("JOINTS_0", JsonValue::Number((*idx).into()));
        }

        if let Some(idx) = boneweight_acessor.as_ref() {
            let _ = gltf["meshes"][mesh_idx]["primitives"][primitive_id]["attributes"]
                .insert("WEIGHTS_0", JsonValue::Number((*idx).into()));
        }
    }
}

// Push the nth buffer named "texcoords" to the mesh,
// if it has such buffer
fn push_texture_coordinate(mesh: &D3DFile, idx: usize, gltf: &mut JsonValue) -> Option<usize> {
    if let Some(uvs) = mesh.nth_buffer(idx, "texcoords") {
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

        Some(uvs_acessor_idx)
    } else {
        None
    }
}

pub fn save_to_gltf(mesh: &D3DFile, output: PathBuf) {
    let mut gltf = empty_gltf();
    add_mesh_to_gltf(mesh, &mut gltf, false, Some(0));

    crate::skl::gltf::save(output, &gltf);
}
