use glam::{quat, vec3};
use std::collections::HashMap;

use json::JsonValue;

use super::Animation;
use crate::formats::{
    gltf::{
        push_accessor, push_animation, push_buffer, push_buffer_view, push_channel_to_animation,
        push_sampler_to_animation, FLOAT,
    },
    skl::Skeleton,
};

pub fn to_gltf(skl: &Skeleton, anm: &Animation, gltf: &mut JsonValue) {
    let track_by_bone_name_hash: HashMap<u64, usize> = anm
        .tracks
        .iter()
        .enumerate()
        .map(|(i, x)| (x.bone_name_hash, i))
        .collect();

    // One buffer contains all keyframe times

    let mut time_buffer: Vec<u8> = vec![];
    let mut time_bone_to_offset = HashMap::new();
    let mut time_bone_to_min = HashMap::new();
    let mut time_bone_to_max = HashMap::new();

    for bone in skl.bones.iter() {
        let track = track_by_bone_name_hash[&bone.name_hash];
        let track = &anm.tracks[track];

        assert!(track.bone_name_hash == bone.name_hash);

        time_bone_to_offset.insert(bone.name_hash, time_buffer.len());
        let min = time_bone_to_min.entry(bone.name_hash).or_insert(f32::MAX);
        let max = time_bone_to_max.entry(bone.name_hash).or_insert(f32::MIN);
        for frame in track.frames.iter() {
            time_buffer.extend(frame.time.to_le_bytes());
            *min = min.min(frame.time);
            *max = max.max(frame.time);
        }
    }

    let time_buffer_view = push_buffer(gltf, time_buffer.as_slice());
    let time_buffer_idx = push_buffer_view(gltf, time_buffer_view);

    // One buffer contains all rotations
    // and other all translations

    let mut rotation_buffer: Vec<u8> = vec![];
    let mut r_bone_to_offset = HashMap::new();
    let mut r_bone_to_min = HashMap::new();
    let mut r_bone_to_max = HashMap::new();
    let mut translation_buffer: Vec<u8> = vec![];
    let mut t_bone_id_to_offset = HashMap::new();
    let mut t_bone_to_min = HashMap::new();
    let mut t_bone_to_max = HashMap::new();

    for bone in skl.bones.iter() {
        let track = track_by_bone_name_hash[&bone.name_hash];
        let track = &anm.tracks[track];

        assert!(track.bone_name_hash == bone.name_hash);

        r_bone_to_offset.insert(bone.name_hash, rotation_buffer.len());
        let r_min = r_bone_to_min.entry(bone.name_hash).or_insert(quat(
            f32::MAX,
            f32::MAX,
            f32::MAX,
            f32::MAX,
        ));
        let r_max = r_bone_to_max.entry(bone.name_hash).or_insert(quat(
            f32::MIN,
            f32::MIN,
            f32::MIN,
            f32::MIN,
        ));

        t_bone_id_to_offset.insert(bone.name_hash, translation_buffer.len());
        let t_min =
            t_bone_to_min
                .entry(bone.name_hash)
                .or_insert(vec3(f32::MAX, f32::MAX, f32::MAX));
        let t_max =
            t_bone_to_max
                .entry(bone.name_hash)
                .or_insert(vec3(f32::MIN, f32::MIN, f32::MIN));

        for (_, frame) in track.frames.iter().enumerate() {
            rotation_buffer.extend(frame.rotation.x.to_le_bytes());
            rotation_buffer.extend(frame.rotation.y.to_le_bytes());
            rotation_buffer.extend(frame.rotation.z.to_le_bytes());
            rotation_buffer.extend(frame.rotation.w.to_le_bytes());

            r_min.x = r_min.x.min(frame.rotation.x);
            r_min.y = r_min.y.min(frame.rotation.y);
            r_min.z = r_min.z.min(frame.rotation.z);
            r_min.w = r_min.w.min(frame.rotation.w);

            r_max.x = r_max.x.max(frame.rotation.x);
            r_max.y = r_max.y.max(frame.rotation.y);
            r_max.z = r_max.z.max(frame.rotation.z);
            r_max.w = r_max.w.max(frame.rotation.w);

            let translation = *bone.length.as_ref().unwrap() * frame.translation.normalize();

            translation_buffer.extend(translation.x.to_le_bytes());
            translation_buffer.extend(translation.y.to_le_bytes());
            translation_buffer.extend(translation.z.to_le_bytes());

            t_min.x = t_min.x.min(translation.x);
            t_min.y = t_min.y.min(translation.y);
            t_min.z = t_min.z.min(translation.z);

            t_max.x = t_max.x.max(translation.x);
            t_max.y = t_max.y.max(translation.y);
            t_max.z = t_max.z.max(translation.z);
        }
    }

    let rot_buffer_view = push_buffer(gltf, rotation_buffer.as_slice());
    let rot_buffer_idx = push_buffer_view(gltf, rot_buffer_view);

    let translation_buffer_view = push_buffer(gltf, translation_buffer.as_slice());
    let translation_buffer_idx = push_buffer_view(gltf, translation_buffer_view);

    // Push all data to gltf

    let mut anim = json::object! {
        name: anm.name.clone()
    };

    for bone in skl.bones.iter() {
        let track = track_by_bone_name_hash[&bone.name_hash];
        let track = &anm.tracks[track];

        assert!(track.bone_name_hash == bone.name_hash);

        if track.frames.len() == 0 {
            continue;
        }

        let time_acessor_idx = push_accessor(
            gltf,
            json::object! {
                bufferView: time_buffer_idx,
                componentType: FLOAT,
                count: track.frames.len(),
                type: "SCALAR",
                byteOffset: time_bone_to_offset[&bone.name_hash],
                min: [time_bone_to_min[&bone.name_hash]],
                max: [time_bone_to_max[&bone.name_hash]],
                extras: {
                    "type": "time buffer",
                    "animation": anm.name.clone()
                }
            },
        );

        let r_min = r_bone_to_min[&bone.name_hash];
        let r_max = r_bone_to_max[&bone.name_hash];
        let rot_acessor_idx = push_accessor(
            gltf,
            json::object! {
                bufferView : rot_buffer_idx,
                componentType : FLOAT,
                count : track.frames.len(),
                type : "VEC4",
                byteOffset: r_bone_to_offset[&bone.name_hash],
                min: [r_min.x, r_min.y, r_min.z, r_min.w],
                max: [r_max.x, r_max.y, r_max.z, r_max.w],
                extras: {
                    "type": "rotation buffer",
                    "animation": anm.name.clone()
                }
            },
        );

        let t_min = t_bone_to_min[&bone.name_hash];
        let t_max = t_bone_to_max[&bone.name_hash];
        let translation_acessor_idx = push_accessor(
            gltf,
            json::object! {
                bufferView: translation_buffer_idx,
                componentType: FLOAT,
                count: track.frames.len(),
                type: "VEC3",
                byteOffset: t_bone_id_to_offset[&bone.name_hash],
                min: [t_min.x, t_min.y, t_min.z],
                max: [t_max.x, t_max.y, t_max.z],
                extras: {
                    type: "translation buffer",
                    animation: anm.name.clone()
                }
            },
        );

        let node_idx = gltf["skins"][0]["joints"][bone.index].as_usize().unwrap();
        let node_name = gltf["nodes"][node_idx]["name"]
            .as_str()
            .unwrap()
            .to_string();

        let rot_sampler_idx = push_sampler_to_animation(
            &mut anim,
            json::object! {
                input : time_acessor_idx,
                interpolation : "LINEAR",
                output : rot_acessor_idx
            },
        );

        push_channel_to_animation(
            &mut anim,
            json::object! {
                sampler: rot_sampler_idx,
                target: json::object!{
                    node: node_idx,
                    path: "rotation"
                },
                extras: {
                    bone: node_name.clone(),
                    animation: anm.name.clone()
                }
            },
        );

        let translation_sampler_idx = push_sampler_to_animation(
            &mut anim,
            json::object! {
                input: time_acessor_idx,
                interpolation: "LINEAR",
                output: translation_acessor_idx
            },
        );
        push_channel_to_animation(
            &mut anim,
            json::object! {
                sampler: translation_sampler_idx,
                target: json::object!{
                    node: node_idx,
                    path: "translation"
                },
                extras: {
                    bone: node_name.clone(),
                    animation: anm.name.clone()
                }
            },
        );
    }

    push_animation(gltf, anim);
}
