use std::path::Path;

use log::trace;

use crate::{
    checksum_mapping::ChecksumMap,
    parser::NomSlice,
    utils::{read_to_end, IOError},
};

#[derive(Debug)]
pub struct Chore {}

pub struct ChoreParser {}

#[derive(Debug)]
pub enum ChoreParserError {
    IO(IOError),
    InvalidMagicNumber,
}

impl ChoreParser {
    pub fn new() -> Self {
        Self {}
    }

    fn parse_resource(&mut self, input: &mut NomSlice) {
        let _ = input.parse_le_u32("?");

        let _ = input.parse_le_u32("Resource Name Section Length");
        let _ = input.parse_length_string("Resource Name");
        let _ = input.parse_le_f32("Length");
        let _ = input.parse_le_u32("Priority");
        let _ = input.parse_le_u32("Flags");
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");

        let _ = input.parse_le_u32("Animation/Chore Length");
        let _ = input.parse_length_string("?");

        let l = input.parse_le_u32("Embedded Data Section Length?");
        let _ = input.parse_n_bytes((l - 4) as usize, "?");

        let _ = input.parse_le_u32("Chore Blocks Section Length");
        let qty = input.parse_le_u32("Chore Blocks Quantity");
        for i in 0..qty {
            trace!("Chore Block: {}", i);
            let _ = input.parse_le_f32("Start Time");
            let _ = input.parse_le_f32("End Time");
            input.parse_n_bytes(1, "Looping");
            let _ = input.parse_le_f32("Scale");
        }
        // let buffer = input.parse_n_bytes((l - 4) as usize, "?");

        input.parse_n_bytes(1, "No Pose");
        input.parse_n_bytes(1, "Embedded");
        input.parse_n_bytes(1, "Enabled");
        input.parse_n_bytes(1, "Is Agent Resource");
        input.parse_n_bytes(1, "View Graphs");
        input.parse_n_bytes(1, "View Empty Graphs");
        input.parse_n_bytes(1, "View Properties");
        input.parse_n_bytes(1, "View Resource Groups");

        let l = input.parse_le_u32("Resource Properties Section Length?");
        let _ = input.parse_n_bytes((l - 4) as usize, "?");

        let _ = input.parse_le_u32("Resource Group Section Length?");
        let qty = input.parse_le_u32("Resource Group Quantity");
        for _ in 0..qty {
            let _ = input.parse_length_string("Resource Group Name");
            input.parse_le_f32("?");
        }

        let _ = input.parse_le_u32("Status");
    }

    fn parse_agent(&mut self, input: &mut NomSlice, mapping: &ChecksumMap) {
        trace!("START parse_agent");
        let _ = input.parse_le_u32("Agent Name Section Length");
        let _ = input.parse_length_string("Agent Name");

        let _ = input.parse_le_u32("Flags");

        let _ = input.parse_le_u32("Resources Section Length");
        let resource_qty = input.parse_le_u32("Resources Quantity");
        for _ in 0..resource_qty {
            let _ = input.parse_le_u32("Resources Index");
        }

        let l = input.parse_le_u32("Attachment Section Length?");
        // let buffer = input.parse_n_bytes((l - 4) as usize, "?");
        let _ = input.parse_n_bytes(1, "Do Attach");
        let _ = input
            .parse_le_u64_with_debug("?", |k| mapping.get_mapping(k).unwrap_or("?".to_string()));
        let _ = input
            .parse_le_u64_with_debug("?", |k| mapping.get_mapping(k).unwrap_or("?".to_string()));
        input.read_vec3("Attach Translation");
        input.read_quat("Attach Rotation");
        let _ = input.parse_n_bytes(1, "Preserve World Position");
        let _ = input.parse_n_bytes(1, "Leave Attachment When Complete");

        let _ = input.parse_le_u32("Section Length?");
        let _ = input.parse_n_bytes((l - 4) as usize, "?");

        trace!("START parse_agent");
    }

    pub fn parse(
        &mut self,
        path: impl AsRef<Path>,
        mapping: &ChecksumMap,
    ) -> Result<Chore, ChoreParserError> {
        let path = path.as_ref();

        let bytes = read_to_end(&path).map_err(ChoreParserError::IO)?;
        let mut input = NomSlice::new(bytes.as_slice());

        if !input.read_ertm_magic_number() {
            return Err(ChoreParserError::InvalidMagicNumber);
        }

        let _ = input.read_properties(mapping);

        let _ = input.parse_le_u32("?");
        let _ = input.parse_length_string("Name");

        input.parse_n_bytes(1, "?");

        let _ = input.parse_le_f32("Length?");
        let resource_qty = input.parse_le_u32("Resource Quantity");
        let agent_qty = input.parse_le_u32("Agent Quantity");

        let l = input.parse_le_u32("Section Length?");
        let _ = input.parse_n_bytes((l - 4) as usize, "?");

        input.parse_n_bytes(1, "?");
        let scene_section = input.parse_n_bytes(1, "Scene Section?");
        if scene_section[0] == 49 {
            let _ = input.parse_le_u32("Scene Section Length");
            let _ = input.parse_length_string("?");
        }

        input.parse_n_bytes(1, "?");
        let _ = input.parse_le_u32("Render Delay?");
        let _ = input
            .parse_le_u64_with_debug("?", |k| mapping.get_mapping(k).unwrap_or("?".to_string()));
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");
        input.parse_n_bytes(1, "?");

        for i in 0..resource_qty {
            trace!("START parse_resource {}", i);
            self.parse_resource(&mut input);
            trace!("END parse_resource");
        }

        for _ in 0..agent_qty {
            self.parse_agent(&mut input, &mapping);
        }

        input.assert_eof();

        todo!()
    }
}
