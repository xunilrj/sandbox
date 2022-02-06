pub mod gltf;

use std::{
    collections::vec_deque,
    io::Read,
    path::{Path, PathBuf},
    str::FromStr,
};

use itertools::Itertools;

use crate::parser::NomSlice;

#[derive(Debug)]
pub struct Bone {
    pub(crate) start: u64,
    pub(crate) end: u64,
    pub(crate) parent: Option<usize>,
    pub(crate) children: Vec<usize>,
    pub(crate) translation: [f32; 3],
    pub(crate) rotation: [f32; 4],
    pub(crate) basis: glam::Mat4,
    pub(crate) global_matrix: glam::Mat4,
    pub(crate) local_matrix: glam::Mat4,
    pub(crate) inverse_bind_pose: glam::Mat4,
}

#[derive(Debug)]
pub struct SklFile {
    pub bones: Vec<Bone>,
}

impl SklFile {
    pub fn new() -> Self {
        Self { bones: Vec::new() }
    }

    pub fn calculate_inverse_bind_pose(&mut self) {}

    pub fn bone_position(&self, bid: u64) -> Option<usize> {
        self.bones.iter().position(|x| x.start == bid)
    }
}

pub fn parse_skl<P: AsRef<Path>>(path: P) -> SklFile {
    let path = path.as_ref().to_str().unwrap();
    let mut f = std::fs::File::open(path).unwrap();
    let bytes = {
        let mut bytes = vec![];
        f.read_to_end(&mut bytes).unwrap();
        bytes
    };

    let mut input = NomSlice::new(bytes.as_slice());

    if !input.read_ertm_magic_number() {
        panic!("ERTM not found")
    }

    let mut skl = SklFile::new();

    let _ = input.read_properties();

    let _ = input.parse_le_u32("?");
    let qty = input.parse_le_u32("Bones Qty") as usize;

    for i in 0..qty {
        let mut ends = vec![];
        for i in 0..2 {
            let k = input.parse_le_u64("Bone ID");
            let _ = input.parse_le_u32("Zero");
            ends.push(k);
        }

        let parent = input.parse_le_u32("parent") as usize;

        let translation = input.read_vec3("translation");
        let rotation = input.read_quat("rotation");

        let l = input.parse_le_u32("Bone Q Section Length");
        assert!(l == 32);

        let _ = input.read_quat("Q quat");
        let _ = input.read_vec3("Q vec");

        let basis = input.read_m33("Bone Basis");

        let _ = input.parse_le_u32("IK Section Length");
        let qty = input.parse_le_u32("IK qty");
        for _ in 0..qty {
            let _ = input.parse_length_string("Bone Name");
            let _ = input.parse_le_f32("weight ?");
        }

        let _ = input.parse_le_f32("?");

        let basis = glam::Mat3::from_cols_array(&basis);
        let basis = glam::Mat4::from_mat3(basis);

        let t = glam::vec3(translation[0], translation[1], translation[2]);
        let t = glam::Mat4::from_translation(t);
        let r = glam::quat(rotation[0], rotation[1], rotation[2], rotation[3]);
        let r = glam::Mat4::from_quat(r);
        let local_matrix = t * r;

        let global_matrix = glam::Mat4::IDENTITY;
        let inverse_bind_pose = glam::Mat4::IDENTITY;
        let bone = Bone {
            start: ends[0],
            end: ends[1],
            parent: if parent == 4294967295 {
                None
            } else {
                Some(parent)
            },
            children: Vec::new(),
            translation,
            rotation,
            basis,
            global_matrix,
            local_matrix,
            inverse_bind_pose,
        };
        skl.bones.push(bone);
        if parent != (u32::MAX as usize) {
            skl.bones[parent].children.push(i);
        }
    }

    let (root, _) = skl
        .bones
        .iter()
        .find_position(|x| x.parent.is_none())
        .unwrap();
    let mut q = std::collections::VecDeque::from([root]);
    while let Some(idx) = q.pop_front() {
        let bone = &skl.bones[idx];

        let parent_global = bone
            .parent
            .map(|p| skl.bones[p].global_matrix)
            .unwrap_or(glam::Mat4::IDENTITY);

        let t: glam::Vec3 = bone.translation.into();
        let t = glam::Mat4::from_translation(t);

        let r = glam::Quat::from_array(bone.rotation);
        let r = glam::Mat4::from_quat(r);

        let bone = &mut skl.bones[idx];
        bone.global_matrix = parent_global * (t * r);
        bone.inverse_bind_pose = bone.global_matrix.inverse();

        println!("global matrix: {:?}", bone.global_matrix.to_cols_array());
        println!("inverse: {:?}", bone.inverse_bind_pose.to_cols_array());

        for child in &bone.children {
            q.push_back(*child);
        }
    }

    skl
}

pub fn convert<P: AsRef<str>>(path: P) {
    let mut bar = progress::Bar::new();

    bar.set_job_title("Parsing...");
    let skl = parse_skl(path.as_ref());

    bar.set_job_title("Saving...");
    let out = PathBuf::from_str(path.as_ref()).unwrap();
    let out = out.with_extension("gltf");
    gltf::convert_and_save(&out, &skl);
}
