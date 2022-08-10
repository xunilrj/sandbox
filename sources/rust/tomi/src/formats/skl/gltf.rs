use json::JsonValue;
use log::warn;

use crate::formats::gltf::{
    get_scene_0_mut, push_accessor, push_buffer, push_buffer_view, push_node, push_node_to_scene,
    push_skin, ARRAY_BUFFER, ELEMENT_ARRAY_BUFFER, FLOAT,
};

use super::Skeleton;

pub fn to_gltf(skl: &Skeleton, gltf: &mut JsonValue) {
    let first_node_id = gltf["nodes"].len();

    let mut inverse_bind_pose_buffer = vec![];

    // Order here is important because of the joints0 buffer
    for bone in &skl.bones {
        let mut node = json::object! {
            name: bone.friendly_name(),
            translation: &bone.local_translation.to_array()[..],
            rotation: &bone.local_rotation.to_array()[..],
            extras: {
                skeleton: skl.name.clone(),
                boneIndex: bone.index,
            }
        };

        if bone.children.len() > 0 {
            let children = bone
                .children
                .iter()
                .map(|x| *x + first_node_id)
                .map(|x| JsonValue::Number((x as isize).into()))
                .collect();
            node["children"] = JsonValue::Array(children);
        }

        push_node(gltf, node);

        if let Some(inverse_bind_pose) = bone.inverse_bind_pose_transformation {
            inverse_bind_pose_buffer.extend(inverse_bind_pose.to_cols_array());
        }
    }

    if inverse_bind_pose_buffer.len() != skl.bones.len() * 16 {
        warn!("No inverse bind pose found");
    } else {
        let inverse_buffer_view = push_buffer(gltf, inverse_bind_pose_buffer.as_slice());
        let inverse_buffer_view_idx = push_buffer_view(gltf, inverse_buffer_view);
        let inverse_bind_pose_acessor_idx = push_accessor(
            gltf,
            json::object! {
                bufferView: inverse_buffer_view_idx,
                componentType: FLOAT,
                byteOffset: 0,
                count: skl.bones.len(),
                type: "MAT4",
                extras: {
                    type: "inverse bind pose",
                    skeleton: skl.name.clone(),
                }
            },
        );

        let joints: Vec<_> = skl
            .bones
            .iter()
            .enumerate()
            .map(|(i, _)| i + first_node_id)
            .collect();

        let skin_id = push_skin(
            gltf,
            json::object! {
                inverseBindMatrices: inverse_bind_pose_acessor_idx,
                joints: joints.as_slice(),
                extras: {
                    skeleton: skl.name.clone(),
                }
            },
        );

        let scene = get_scene_0_mut(gltf);
        push_node_to_scene(scene, first_node_id);
    }
}
