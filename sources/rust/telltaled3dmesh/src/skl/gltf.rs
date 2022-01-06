use std::path::Path;

use json::{number::Number, JsonValue, value};

pub fn to_gltf(skl: &super::SklFile) -> json::JsonValue {
    let asset = json::object! {
        generator: "telltale-converter",
        version: "2.0"
    };

    let scenes = json::array! [{
        nodes: [1]
    }];

    let mut nodes = json::array! [{
        skin: 0,
        // mesh: 0,
    }];

    for bone in &skl.bones {
        let children: Vec<_> = bone.children.iter().map(|x| *x + 1).collect();
        let _ = nodes.push(json::object! {
            translation: &bone.translation[..],
            rotation: &bone.rotation[..],
            children: children.as_slice(),
        });
    }

    let joints: Vec<_> = skl.bones.iter().enumerate().map(|(i, _)| i + 1).collect();
    let skins = json::array![{
        inverseBindMatrices: 4,
        joints: joints.as_slice()
    }];

    let mut gltf = json::object! {
        asset: asset,
        scene: 0,
        scenes: scenes,
        nodes: nodes,
        skins: skins,
    };

    let mut inverse_bind_pose = vec![];
    for bone in &skl.bones {
        let data: &[f32;16] = bone.inverse_bind_pose.as_ref();
        inverse_bind_pose.extend_from_slice(&data[..])
    }
    let mut inverse_buffer_view = crate::gltf::push_buffer(&mut gltf, inverse_bind_pose.as_slice());
    inverse_buffer_view["target"] = JsonValue::Number(34963i32.into());
    let inverse_bfview_idx = crate::gltf::push_buffer_view(&mut gltf, inverse_buffer_view);
    crate::gltf::push_accessor(&mut gltf, 
        json::object! {
            bufferView: inverse_bfview_idx,
            componentType: 5126,
            offset: 0,
            count: skl.bones.len(),
            type: "MAT4",
        }
    );

    gltf
}

pub fn save<P: AsRef<Path>>(path: P, gltf: &json::JsonValue) {
    let _ = std::fs::write(path, gltf.dump());
}

pub fn convert_and_save<P: AsRef<Path>>(path: P, skl: &super::SklFile) {
    let gltf = to_gltf(skl);
    save(path, &gltf);
}
