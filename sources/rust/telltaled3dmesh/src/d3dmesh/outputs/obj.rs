use super::super::d3dfile::D3DFile;
use std::io::Write;
use std::{fs::File, path::PathBuf};

#[inline(always)]
fn obj_o(f: &mut File, name: &str) {
    let _ = write!(f, "o {}\n", name);
}

#[inline(always)]
fn obj_g(f: &mut File, name: &str) {
    let _ = write!(f, "g {}\n", name);
}

#[inline(always)]
fn obj_v(f: &mut File, x: f32, y: f32, z: f32) {
    let _ = write!(f, "v {:.32} {:.32} {:.32}\n", x, y, z);
}

#[inline(always)]
fn obj_f(f: &mut File, a: u16, b: u16, c: u16) {
    let _ = write!(f, "f {} {} {}\n", a, b, c);
}

pub fn save_to_obj(mesh: &D3DFile, path: PathBuf) {
    let _ = std::fs::remove_file(&path);
    let mut f = std::fs::File::create(&path).unwrap();

    obj_o(&mut f, "obj");

    let vertices = mesh
        .buffers
        .iter()
        .find(|x| x.r#type == "position")
        .unwrap();
    let vertices: Vec<_> = vertices.as_f32().chunks(3).collect();
    for v in vertices {
        obj_v(&mut f, v[0], v[1], v[2])
    }

    let indices = mesh.buffers.iter().find(|x| x.r#type == "index").unwrap();
    let indices = indices.as_u16();

    for (i, m) in mesh.meshes.iter().enumerate() {
        obj_g(&mut f, format!("group_{}", i).as_str());

        let start = m.index_start;
        let end = start + (m.tri_count * 3);
        let idxs = (start..end).step_by(3);
        for fi in idxs {
            obj_f(
                &mut f,
                indices[fi + 0] + 1,
                indices[fi + 1] + 1,
                indices[fi + 2] + 1,
            );
        }

        let _ = write!(f, "\n");
    }
}
