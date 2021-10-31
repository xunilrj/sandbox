use std::path::Path;

pub fn save<P: AsRef<Path>>(path: P, skl: super::SklFile) {
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
        nodes.push(json::object! {
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

    let gltf = json::object! {
        asset: asset,
        scene: 0,
        scenes: scenes,
        nodes: nodes,
        skins: skins
    };

    std::fs::write("./viewer/models/result.gltf", gltf.dump());
}
