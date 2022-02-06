use std::{io::Read, path::Path, str::FromStr};

use itertools::Itertools;
use log::debug;

use crate::{
    d3dmesh::d3dfile::{D3DBoundingBox, D3DBufferData, D3DFile, D3DMesh},
    parser::{NomSlice, ParseD3dMeshError},
};

use self::d3dfile::D3DBuffer;

mod attributes;
mod d3dfile;
mod indexbuffer;
pub mod outputs;

pub fn parse_d3dmesh<S: AsRef<Path>>(
    path: S,
    skl: Option<&crate::skl::SklFile>,
) -> Result<D3DFile, &'static str> {
    let path = path.as_ref().to_str().unwrap();
    let mut d3dfile = D3DFile::new();

    let mut f = std::fs::File::open(path).unwrap();
    let bytes = {
        let mut bytes = vec![];
        f.read_to_end(&mut bytes).unwrap();
        bytes
    };

    let mut input = NomSlice::new(bytes.as_slice());

    if !input.read_ertm_magic_number() {
        return Err("ERTM not found");
    }

    let param_count = input.parse_le_u32("?");
    let _param_hash = input.parse_n_bytes(8);

    for _ in 0..(param_count - 1) {
        let _ = input.parse_le_u32("?");
        let _ = input.parse_n_bytes(8);
    }

    let _ = input.parse_le_u32("?");
    let _header_len = input.parse_le_u32("?");
    let d3d_name = input.parse_length_string("?");

    d3dfile.name = d3d_name.to_string();

    let _ = input.parse_length_string("?");

    let minx = input.parse_le_f32("bbox.minx");
    let miny = input.parse_le_f32("bbox.miny");
    let minz = input.parse_le_f32("bbox.minz");
    let maxx = input.parse_le_f32("bbox.maxx");
    let maxy = input.parse_le_f32("bbox.maxy");
    let maxz = input.parse_le_f32("bbox.maxz");
    d3dfile.bbox = D3DBoundingBox {
        minx: minx,
        miny: miny,
        minz: minz,
        maxx: maxx,
        maxy: maxy,
        maxz: maxz,
    };

    let _header_size = input.parse_le_u32("?");
    let qty_meshes = input.parse_le_u32("?");

    for i in 0..qty_meshes {
        debug!("Mesh: {}", i);

        let mut mesh = D3DMesh::new();

        input.qty = 0;

        let _header_hash = input.parse_le_u64("Mesh header hash");

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u64("?");

        let _ = input.parse_le_u32("?");
        mesh.bone_pallete = input.parse_le_u32("Bone Pallete") as usize;
        let _ = input.parse_le_u32("?");

        mesh.vertices[0] = input.parse_le_u32("Vertex Start") as usize;
        mesh.vertices[1] = input.parse_le_u32("Vertex End") as usize;

        mesh.index_start = input.parse_le_u32("Index Start") as usize;
        mesh.tri_count = input.parse_le_u32("Tri Count") as usize;
        let _ = input.parse_le_u64("section header");

        for _ in 0..9 {
            let att = attributes::read_att(&mut input);
            match att {
                attributes::Attribute::BoundingBox(minx, miny, minz, maxx, maxy, maxz) => {
                    mesh.bbox = D3DBoundingBox {
                        minx,
                        miny,
                        minz,
                        maxx,
                        maxy,
                        maxz,
                    }
                }
                _ => {}
            }
        }

        let _ = input.parse_n_bytes(1);
        let _ = attributes::read_att(&mut input);
        let _ = input.parse_le_u32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_u32("?");

        // material and parameters?
        let _ = input.parse_n_bytes(1);
        let _ = attributes::read_att(&mut input);
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.parse_n_bytes(1);
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

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);

        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_u32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);

        let _ = input.parse_length_string("?");
        let _ = input.parse_length_string("?");

        d3dfile.meshes.push(mesh);
    }

    for _ in 0..3 {
        let prop = attributes::read_att2(&mut input);
        match prop {
            attributes::Attribute::BonePallete(pallete) => d3dfile.palletes.extend(pallete),
            _ => {}
        }
    }

    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);

    let _ = input.parse_le_u32("?");

    let _ = input.parse_n_bytes(1);
    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u32("?");

    for _ in 0..2 {
        let _ = attributes::read_att(&mut input);
    }

    let _ = input.parse_n_bytes(1);

    // Buffers

    let _ = input.parse_le_u32("?");
    let qty_indices = input.parse_le_u32("IB Qty Indices");
    let a = input.parse_le_u32("?"); //              ESP+18
    let first_index = input.parse_le_u16("IB First Index"); // first index? ESP+14

    let buffer_size = input.parse_le_u32("IB Buffer Size");
    let index_buffer: Vec<_> = input
        .parse_n_bytes(buffer_size as usize)
        .iter()
        .map(|x| *x)
        .collect();

    let mut esp14 = first_index as u32;
    let mut esp18 = a;
    let mut esp20 = 0;
    let mut esp40 = 0;
    let mut esi1c = qty_indices;
    let mut esi20 = 2;
    let index_buffer_bytes = indexbuffer::get_index_buffer(
        index_buffer,
        &mut esp14,
        &mut esp18,
        &mut esp20,
        &mut esp40,
        &mut esi1c,
        &mut esi20,
    );
    debug!("converting index buffer");
    let index_buffer = unsafe {
        std::slice::from_raw_parts(
            index_buffer_bytes.as_ptr() as *const u16,
            index_buffer_bytes.len() / 2,
        )
    };

    debug!("writing index buffer");
    d3dfile.buffers.push(D3DBuffer {
        r#type: "index".to_string(),
        qty: qty_indices,
        stride: 2,
        data: D3DBufferData::U16(index_buffer.to_vec()),
    });
    debug!("done.");

    // Read buffers

    while input.slice.len() != 0 {
        let mut buffer = parse_d3dmesh_buffer(&mut input);

        if buffer.r#type == "bone_idx" {
            if let Some(skl) = skl {
                let mut bone_indices = buffer.as_u8_mut();

                let indices = d3dfile.get_buffer("index");
                let indices = indices.as_u16();
                for m in d3dfile.meshes.iter() {
                    let start = m.index_start;
                    let end = start + (m.tri_count * 3);
                    for fi in start..end {
                        let vertex_index = indices[fi] as usize;
                        fix_bone_index(&d3dfile, m, &mut bone_indices[vertex_index * 4 + 0], skl);
                        fix_bone_index(&d3dfile, m, &mut bone_indices[vertex_index * 4 + 1], skl);
                        fix_bone_index(&d3dfile, m, &mut bone_indices[vertex_index * 4 + 2], skl);
                        fix_bone_index(&d3dfile, m, &mut bone_indices[vertex_index * 4 + 3], skl);
                    }
                }
            }
        }

        d3dfile.buffers.push(buffer);
    }

    Ok(d3dfile)
}

fn fix_bone_index(d3dfile: &D3DFile, m: &D3DMesh, bone_index: &mut u8, skl: &crate::skl::SklFile) {
    let pallete = &d3dfile.palletes[m.bone_pallete];
    if let Some(bone) = pallete.bones.get(*bone_index as usize) {
        let bone = skl.bone_position(*bone as u64).unwrap();
        *bone_index = bone as u8;
    }
}

pub fn convert<S: AsRef<Path>>(
    path: S,
    output: Option<String>,
    pretty_print: bool,
    buffer_as_base64: bool,
    _detach_index_buffer: bool,
) -> std::result::Result<(), ParseD3dMeshError> {
    let mut bar = progress::Bar::new();
    bar.set_job_title("Parsing...");

    let path = path.as_ref();
    let d3dfile = parse_d3dmesh(path, None).unwrap();

    // let b4 = d3dfile.buffers.iter().find(|x| x.r#type == "bone_weigth?").unwrap();
    // let b5 = d3dfile.buffers.iter().find(|x| x.r#type == "5").unwrap();

    // let w = b4.as_f32();
    // let i  = b5.as_u32();
    // for (w, i) in w.iter().zip(i.iter()) {
    //     println!("{} {}", *w, *i);
    // }

    bar.reach_percent(100);
    let mut bar = progress::Bar::new();
    bar.set_job_title("Saving...");
    match output {
        Some(output) => {
            outputs::save_to(&d3dfile, output.as_str(), buffer_as_base64, pretty_print);

            let output = std::path::PathBuf::from_str(output.as_str()).unwrap();
            let _output = output.with_extension("ib");

            // if detach_index_buffer {
            //     use std::str::FromStr;
            //     std::fs::write(output, index_buffer_bytes);
            // }
        }
        None => {
            todo!("print to stdout")
            // if !pretty_print {
            //     println!("{}", json);
            // } else {
            //     println!("{}", json.pretty(4));
            // }
        }
    }

    bar.set_job_title("Done");
    bar.reach_percent(100);

    Ok(())
}

fn parse_d3dmesh_buffer(input: &mut NomSlice) -> D3DBuffer {
    input.qty = 0;

    let qty = input.parse_le_u32("Buffer Qty");
    let stride = input.parse_le_u32("Buffer Stride");
    let t = input.parse_le_u32("Buffer Type");
    let _ = input.parse_le_u32("?");
    let data = match t {
        4 => {
            use itertools::*;
            let data = input.parse_f32_slice(((stride * qty) / 4) as usize);
            let mut newdata = vec![];
            for chunk in &data.iter().chunks(3) {
                let chunk = chunk.collect::<Vec<_>>();
                newdata.extend([*chunk[0], *chunk[1], *chunk[2], 0.0]);
            }
            D3DBufferData::F32(newdata)
        }
        5 => {
            let data = input.parse_u32_slice(qty as usize);
            let mut newdata = vec![];
            for v in data {
                let a = (v & 0x000000FF) >> 0 >> 2;
                let b = (v & 0x0000FF00) >> 8 >> 2;
                let c = (v & 0x00FF0000) >> 16 >> 2;
                let d = (v & 0xFF000000) >> 24 >> 2;
                newdata.extend(&[a as u8, b as u8, c as u8, 0u8]);
            }
            D3DBufferData::U8(newdata)
        }
        _ => {
            let data = input.parse_f32_slice(((stride * qty) / 4) as usize);
            D3DBufferData::F32(data.to_vec())
        }
    };

    let t = match t {
        1 => "position".to_string(),
        2 => "normal".to_string(),
        4 => "bone_weigth".to_string(),
        5 => "bone_idx".to_string(),
        _ => format!("{}", t),
    };

    D3DBuffer {
        r#type: t,
        qty,
        stride,
        data,
    }
}
