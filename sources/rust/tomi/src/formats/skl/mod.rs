pub mod gltf;

use glam::{vec3, Mat4, Quat, Vec3};
use log::info;

use crate::{
    checksum_mapping::ChecksumMap,
    parser::NomSlice,
    utils::{read_to_end, IOError},
};
use std::{
    collections::{BTreeSet, HashMap, VecDeque},
    path::Path,
};

pub struct SklParser {}

pub struct Skeleton {
    pub name: String,
    pub bones: Vec<Bone>,
}

impl Skeleton {
    pub fn calculate_inverse_bind_pose(&mut self) {
        let mut q = VecDeque::from_iter([0]);
        while let Some(i) = q.pop_front() {
            let parent_global = self.bones[i]
                .parent_index
                .and_then(|p| self.bones[p].bind_pose_transformation.clone())
                .unwrap_or(Mat4::IDENTITY);
            let bone = &mut self.bones[i];

            let bind_pose_transformation = parent_global * bone.local_transformation;
            let mut inverse_bind_pose_transformation = bind_pose_transformation.inverse();
            inverse_bind_pose_transformation.w_axis.w = 1.0;

            bone.inverse_bind_pose_transformation = Some(inverse_bind_pose_transformation);
            bone.bind_pose_transformation = Some(bind_pose_transformation);

            q.extend(bone.children.iter());
        }
    }
}

pub struct Bone {
    pub index: usize,
    pub name_hash: u64,
    pub name: Option<String>,
    pub parent_name_hash: u64,
    pub parent_name: Option<String>,
    pub parent_index: Option<usize>,
    pub local_translation: Vec3,
    pub local_rotation: Quat,
    pub global_translation_scale: Vec3,
    pub local_translation_scale: Vec3,
    pub anim_translation_scale: Vec3,
    pub children: Vec<usize>,
    pub local_transformation: Mat4,
    pub bind_pose_transformation: Option<Mat4>,
    pub inverse_bind_pose_transformation: Option<Mat4>,
    pub groups: BTreeSet<String>,
}

impl Bone {
    pub fn friendly_name(&self) -> String {
        match &self.name {
            Some(name) => name.clone(),
            None => format!("{}", self.name_hash),
        }
    }
}

#[derive(Debug)]
pub enum SklParserError {
    IO(IOError),
    InvalidMagicNumber,
}

impl SklParserError {
    pub(crate) fn user_facing_reason(&self) -> String {
        match self {
            SklParserError::IO(e) => {
                format!(
                    "{} when acessing [{}]",
                    e.error.to_string(),
                    e.path.display()
                )
            }
            SklParserError::InvalidMagicNumber => "This file magic number is not ERTM".to_string(),
        }
    }
}

impl SklParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(
        &mut self,
        path: impl AsRef<Path>,
        mapping: &ChecksumMap,
    ) -> Result<Skeleton, SklParserError> {
        let path = path.as_ref();

        let bytes = read_to_end(&path).map_err(SklParserError::IO)?;
        let mut input = NomSlice::new(bytes.as_slice());

        if !input.read_ertm_magic_number() {
            return Err(SklParserError::InvalidMagicNumber);
        }

        let _properties = input.read_properties(&mapping);

        let _ = input.parse_le_u32("Skeleton Section Length");
        let bone_qty = input.parse_le_u32("Bones Qty") as usize;

        let mut bones: Vec<Bone> = vec![];
        for i in 0..bone_qty {
            let index = bones.len();

            info!("Parsing bone: {}", i);
            let bone = BoneParser::parse(&mut input, index, mapping).unwrap();

            if let Some(i) = bone.parent_index {
                bones[i].children.push(index);
            }

            bones.push(bone);
        }

        input.assert_eof();

        for bone in bones.iter() {
            println!(
                "{:?},{},{},{}",
                bone.name.clone().unwrap_or(format!("{}", bone.name_hash)),
                bone.anim_translation_scale.x,
                bone.anim_translation_scale.y,
                bone.anim_translation_scale.z
            );
        }

        let name = path.file_stem().unwrap().to_str().unwrap().to_string();
        Ok(Skeleton { name, bones })
    }
}

struct BoneParser {}

impl BoneParser {
    pub fn parse<'a>(
        input: &mut NomSlice<'a>,
        index: usize,
        mapping: &ChecksumMap,
    ) -> Result<Bone, SklParserError> {
        let name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
            mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
        });
        let name = mapping.get_mapping(name_hash);

        let v = input.parse_le_u32("?");
        assert!(v == 0);

        let parent_name_hash = input.parse_le_u64_with_debug("Parent Bone Name Hash", |k| {
            mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
        });
        let parent_name = mapping.get_mapping(name_hash);

        let v = input.parse_le_u32("?");
        assert!(v == 0);

        let parent_index = input.parse_le_u32("Parent Bone Index") as usize;
        let parent_index = if parent_index == 4294967295 {
            None
        } else {
            Some(parent_index)
        };

        let local_translation = input.read_vec3("Local Translation");
        let local_rotation = input.read_quat("Local Rotation");

        let l = input.parse_le_u32("Rest X Form Section Length");
        assert!(l == 32);
        let _ = input.read_quat("Rest X form Rotation");
        let _ = input.read_vec3("Rest X form Translation");

        let global_translation_scale = input.read_vec3("Global Translation Scale");
        let local_translation_scale = input.read_vec3("Local Translation Scale");
        let anim_translation_scale = input.read_vec3("Anim Translation Scale");

        let mut groups = BTreeSet::new();
        let _ = input.parse_le_u32("Group Section Length");
        let qty = input.parse_le_u32("Group Quantity");
        for _ in 0..qty {
            let name = input.parse_length_string("Group Name").to_string();
            groups.insert(name);
            let _ = input.parse_le_f32("?");
        }

        let _ = input.parse_le_f32("?");

        let local_transformation = Mat4::from_scale_rotation_translation(
            Vec3::ONE,
            local_rotation.clone(),
            local_translation,
        );

        Ok(Bone {
            index,
            name_hash,
            name,
            parent_name_hash,
            parent_name,
            parent_index,
            local_translation,
            local_rotation,
            global_translation_scale,
            local_translation_scale,
            anim_translation_scale,
            children: vec![],
            local_transformation,
            bind_pose_transformation: None,
            inverse_bind_pose_transformation: None,
            groups,
        })
    }
}
