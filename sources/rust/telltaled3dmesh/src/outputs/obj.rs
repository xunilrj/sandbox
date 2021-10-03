use crate::d3dfile::D3DFile;
use std::io::Write;
use std::{fs::File, path::PathBuf};

#[inline(always)]
fn obj_v(f: &mut File, x: f32, y: f32, z: f32) {
    let _ = write!(f, "v {} {} {}\n", x, y, z);
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

    let vertices = mesh
        .buffers
        .iter()
        .find(|x| x.r#type == "position")
        .unwrap();
    let vertices = vertices.as_f32();
    for v in vertices.chunks(3) {
        obj_v(&mut f, v[0], v[1], v[2]);
    }

    let indices = mesh.buffers.iter().find(|x| x.r#type == "index").unwrap();
    let indices = indices.as_u16();
    for i in indices.chunks(3) {
        obj_f(&mut f, i[0] + 1, i[1] + 1, i[2] + 1);
    }
}
