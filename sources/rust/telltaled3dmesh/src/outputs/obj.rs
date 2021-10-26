use crate::d3dfile::D3DFile;
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
    let mut f = File::with_options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .unwrap();

    obj_o(&mut f, "obj");

    let vertices = mesh
        .buffers
        .iter()
        .find(|x| x.r#type == "position")
        .unwrap();
    let vertices: Vec<_> = vertices.as_f32().chunks(3).collect();

    for vi in vertices {
        obj_v(&mut f, vi[0], vi[1], vi[2])
    }

    let indices = mesh.buffers.iter().find(|x| x.r#type == "index").unwrap();
    let indices = indices.as_u16();

    for (i, m) in mesh.meshes.iter().enumerate() {
        obj_g(&mut f, format!("group_{}", i).as_str());

        let idxs: Vec<_> = (m.indices[0]..m.indices[1]).collect();
        for fi in idxs.chunks(3) {
            if fi.len() != 3 {
                continue;
            }
            obj_f(
                &mut f,
                indices[fi[0]] + 1,
                indices[fi[1]] + 1,
                indices[fi[2]] + 1,
            );
        }

        write!(f, "\n\n");
    }
}
