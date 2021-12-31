use std::{io::Read, str::FromStr};

use log::debug;

use crate::{
    d3dmesh::d3dfile::{D3DBoundingBox, D3DBufferData, D3DFile, D3DMesh},
    parser::{NomSlice, ParseD3dMeshError},
};

use self::d3dfile::D3DBuffer;

mod attributes;
mod d3dfile;
mod indexbuffer;
mod outputs;

pub fn convert<S: AsRef<str>>(
    path: S,
    output: Option<String>,
    pretty_print: bool,
    buffer_as_base64: bool,
    _detach_index_buffer: bool,
) -> std::result::Result<(), ParseD3dMeshError> {
    let mut bar = progress::Bar::new();
    bar.set_job_title("Parsing...");

    let mut d3dfile = D3DFile::new();

    let mut f = std::fs::File::open(path.as_ref()).unwrap();
    let bytes = {
        let mut bytes = vec![];
        f.read_to_end(&mut bytes).unwrap();
        bytes
    };

    let mut input = NomSlice::new(bytes.as_slice());

    if !input.read_ertm_magic_number() {
        panic!("ERTM not found")
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
        bar.reach_percent(25);
        let mut mesh = D3DMesh::new();

        input.qty = 0;

        let _header_hash = input.parse_le_u64("Mesh header hash");

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u64("?");

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");
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
        let _ = attributes::read_att2(&mut input);
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

    bar.reach_percent(50);

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
        bar.reach_percent(75);
        let buffer = parse_d3dmesh_buffer(&mut input);
        d3dfile.buffers.push(buffer);
    }

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
    let data = input.parse_f32_slice(((stride * qty) / 4) as usize);

    let t = match t {
        1 => "position".to_string(),
        2 => "normal".to_string(),
        4 => "bone_weigth?".to_string(),
        _ => format!("{}", t),
    };

    D3DBuffer {
        r#type: t,
        qty,
        stride,
        data: D3DBufferData::F32(data.to_vec()),
    }
}
