mod attributes;

use crate::{
    checksum_mapping::ChecksumMap,
    parser::{whats_next, NomSlice},
    utils::{read_to_end, IOError},
};
use log::info;
use std::path::Path;

pub struct Mesh {}

#[derive(Debug)]
pub enum MeshParserError {
    IO(IOError),
    InvalidMagicNumber,
}
pub struct D3dMeshParser {}

impl D3dMeshParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(
        &mut self,
        path: impl AsRef<Path>,
        mapping: &ChecksumMap,
    ) -> Result<Mesh, MeshParserError> {
        let path = path.as_ref();

        let bytes = read_to_end(&path).map_err(MeshParserError::IO)?;
        let mut input = NomSlice::new(bytes.as_slice());

        if !input.read_ertm_magic_number() {
            return Err(MeshParserError::InvalidMagicNumber);
        }

        let _properties = input.read_properties(&mapping);

        let _header_len = input.parse_le_u32("Name Section Length");
        let d3d_name = input.parse_length_string("Name");

        let _ = input.parse_length_string("Version ?");

        let minx = input.parse_le_f32("Bounding Box Min x");
        let miny = input.parse_le_f32("Bounding Box Min y");
        let minz = input.parse_le_f32("Bounding Box Min z");
        let maxx = input.parse_le_f32("Bounding Box Max x");
        let maxy = input.parse_le_f32("Bounding Box Max y");
        let maxz = input.parse_le_f32("Bounding Box Max z");

        let _ = input.parse_le_u32("Mesh Section Length");
        let qty_meshes = input.parse_le_u32("Mesh Quantity");

        for i in 0..qty_meshes {
            info!("Mesh: {}", i);

            let name_hash = input.parse_le_u64_with_debug("mPixelShaderName?", |k| {
                mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
            });
            let zero = input.parse_le_u32("?");
            assert!(zero == 0);

            let _ = input.parse_le_u64_with_debug("mpVertexShader?", |k| {
                mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
            });
            let zero = input.parse_le_u32("?") as usize;
            assert!(zero == 0);

            let bone_pallete = input.parse_le_u32("Bone Pallete") as usize;
            let _ = input.parse_le_u32("mGeometryFormat?") as usize;

            let _ = input.parse_le_u32("Vertex Start") as usize;
            let _ = input.parse_le_u32("Vertex End") as usize;
            let _ = input.parse_le_u32("Index Start") as usize;
            let _ = input.parse_le_u32("Tri Count") as usize;

            let _ = input.parse_le_u64_with_debug("mLightingGroup?", |k| {
                mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
            });
            let zero = input.parse_le_u32("?");
            assert!(zero == 0);

            let minx = input.parse_le_f32("bbox.minx");
            let miny = input.parse_le_f32("bbox.miny");
            let minz = input.parse_le_f32("bbox.minz");
            let maxx = input.parse_le_f32("bbox.maxx");
            let maxy = input.parse_le_f32("bbox.maxy");
            let maxz = input.parse_le_f32("bbox.maxz");

            // Bounding Sphere?

            let l = input.parse_le_u32("mBoundingSphere Section Length?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");

            // Texture Maps?

            let l = input.parse_le_u32("mhDiffuseMap Section Length?");
            let _ = input.parse_length_string("?");

            let l = input.parse_le_u32("mhDetailMap Section Length?");
            let _ = input.parse_length_string("?");

            let l = input.parse_le_u32("mhBumpMap Section Length?");
            let _ = input.parse_length_string("?");

            let l = input.parse_le_u32("mhEnvMap Section Length?");
            let _ = input.parse_length_string("?");

            let l = input.parse_le_u32("mhSpecularColorMap Section Length?");
            let _ = input.parse_length_string("?");

            let l = input.parse_le_u32("mhAmbientMap Section Length?");
            let _ = input.parse_length_string("?");

            let l = input.parse_le_u32("mhToonLightQuantized Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_n_bytes(1, "mbToonRendering?");
            let _ = attributes::read_att(&mut input);
            let _ = input.parse_le_u32("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_le_u32("?");

            let _ = input.parse_n_bytes(1, "?");
            let l = input.parse_le_u32("Section Length?");
            let _ = input.parse_length_string("?");
            let _ = input.read_vec3("?");
            let _ = input.read_vec3("?");
            let _ = input.read_vec3("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");

            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_u32("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_le_u32("?");
            let _ = input.parse_le_f32("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");

            let _ = input.parse_length_string("?");
            let _ = input.parse_length_string("?");
        }
        todo!();
        input.assert_eof();

        Ok(Mesh {})
    }
}
