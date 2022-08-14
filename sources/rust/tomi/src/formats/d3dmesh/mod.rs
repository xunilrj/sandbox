use crate::{
    checksum_mapping::ChecksumMap,
    parser::NomSlice,
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
        let _ = input.parse_length_string("Name");

        let _ = input.parse_length_string("Version ?");

        let _ = input.parse_le_f32("Bounding Box Min x");
        let _ = input.parse_le_f32("Bounding Box Min y");
        let _ = input.parse_le_f32("Bounding Box Min z");
        let _ = input.parse_le_f32("Bounding Box Max x");
        let _ = input.parse_le_f32("Bounding Box Max y");
        let _ = input.parse_le_f32("Bounding Box Max z");

        let _ = input.parse_le_u32("Mesh Section Length");
        let qty_meshes = input.parse_le_u32("Mesh Quantity");

        for i in 0..qty_meshes {
            info!("Mesh: {}", i);

            let _ = input.parse_le_u64_with_debug("mPixelShaderName?", |k| {
                mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
            });
            let zero = input.parse_le_u32("?");
            assert!(zero == 0);

            let _ = input.parse_le_u64_with_debug("mpVertexShader?", |k| {
                mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
            });
            let zero = input.parse_le_u32("?") as usize;
            assert!(zero == 0);

            let _ = input.parse_le_u32("Bone Pallete") as usize;
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

            let _ = input.parse_le_f32("bbox.minx");
            let _ = input.parse_le_f32("bbox.miny");
            let _ = input.parse_le_f32("bbox.minz");
            let _ = input.parse_le_f32("bbox.maxx");
            let _ = input.parse_le_f32("bbox.maxy");
            let _ = input.parse_le_f32("bbox.maxz");

            // Bounding Sphere?

            let _ = input.parse_le_u32("mBoundingSphere Section Length?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");

            // Texture Maps?

            let _ = input.parse_le_u32("mhDiffuseMap Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_le_u32("mhDetailMap Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_le_u32("mhBumpMap Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_le_u32("mhEnvMap Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_le_u32("mhSpecularColorMap Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_le_u32("mhAmbientMap Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_le_u32("mhToonLightQuantized Section Length?");
            let _ = input.parse_length_string("?");

            let _ = input.parse_n_bytes(1, "mbToonRendering?");
            // let _ = attributes::read_att(&mut input);
            let _ = input.parse_le_u32("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_le_u32("?");

            let _ = input.parse_n_bytes(1, "?");
            let _ = input.parse_le_u32("Section Length?");
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

        let _ = input.parse_le_u32("Section Length");
        let _ = input.parse_length_string("?");

        let _ = input.parse_le_u32("Bone Palettes Section Length");
        let palettes_qty = input.parse_le_u32("Palettes Qty");
        for _ in 0..palettes_qty {
            let bone_qty = input.parse_le_u32("Bones Quantity");
            for _ in 0..bone_qty {
                let _ = input.parse_le_u64_with_debug("Bone Name Hash?", |k| {
                    mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
                });
                let zero = input.parse_le_u32("zero");
                assert!(zero == 0);
            }
        }

        let _ = input.parse_le_u32("Section Length");
        let _ = input.parse_length_string("?");

        let _ = input.parse_n_bytes(1, "?");
        let _ = input.parse_n_bytes(1, "?");
        let _ = input.parse_n_bytes(1, "?");
        let _ = input.parse_n_bytes(1, "?");

        let _ = input.parse_le_u32("?");

        let _ = input.parse_n_bytes(1, "?");
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");

        let _ = input.parse_le_u32("Section Length");
        let _ = input.parse_length_string("?");

        let _ = input.parse_le_u32("Section Length");
        let _ = input.parse_length_string("?");

        let _ = input.parse_n_bytes(1, "?");

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("IB Qty Indices");
        let _ = input.parse_le_u32("?"); //              ESP+18
        let _ = input.parse_le_u16("IB First Index"); // first index? ESP+14

        let buffer_size = input.parse_le_u32("IB Buffer Size");
        let _: Vec<_> = input
            .parse_n_bytes(buffer_size as usize, "index buffer")
            .iter()
            .map(|x| *x)
            .collect();

        while input.slice.len() != 0 {
            let qty = input.parse_le_u32("Buffer Qty");
            let stride = input.parse_le_u32("Buffer Stride");
            let t = input.parse_le_u32("Buffer Type");
            let _ = input.parse_le_u32("?");

            match t {
                4 => {
                    use itertools::*;
                    let data = input.parse_f32_slice(((stride * qty) / 4) as usize);
                    let mut newdata = vec![];
                    for chunk in &data.iter().chunks(3) {
                        let chunk = chunk.collect::<Vec<_>>();
                        newdata.extend([*chunk[0], *chunk[1], *chunk[2], 0.0]);
                    }
                }
                5 => {
                    let data = input.parse_u32_slice(qty as usize);
                    let mut newdata: Vec<u8> = vec![];
                    for v in data {
                        let a = (v & 0x000000FF) >> 0 >> 2;
                        let b = (v & 0x0000FF00) >> 8 >> 2;
                        let c = (v & 0x00FF0000) >> 16 >> 2;
                        let _ = (v & 0xFF000000) >> 24 >> 2;
                        newdata.extend(&[a as u8, b as u8, c as u8, 0u8]);
                    }
                }
                _ => {
                    let _ = input.parse_f32_slice(((stride * qty) / 4) as usize);
                }
            };
        }

        input.assert_eof();

        Ok(Mesh {})
    }
}
