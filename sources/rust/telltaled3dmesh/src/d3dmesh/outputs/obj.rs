use super::super::d3dfile::D3DFile;
use std::collections::HashSet;
use std::io::Write;
use std::{fs::File, path::PathBuf};

//http://paulbourke.net/dataformats/obj/minobj.html

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
fn obj_vn(f: &mut File, x: f32, y: f32, z: f32) {
    let _ = write!(f, "vn {:.32} {:.32} {:.32}\n", x, y, z);
}

#[inline(always)]
fn obj_vt(f: &mut File, x: f32, y: f32) {
    let _ = write!(f, "vt {:.32} {:.32}\n", x, y);
}

#[inline(always)]
fn obj_f_v(f: &mut File, a: u16, b: u16, c: u16) {
    let _ = write!(f, "f {} {} {}\n", a, b, c);
}

#[inline(always)]
fn obj_f_vn(f: &mut File, va: u16, vb: u16, vc: u16, na: u16, nb: u16, nc: u16) {
    let _ = write!(f, "f {va}/{na} {vb}/{nb} {vc}/{nc}\n");
}

#[inline(always)]
fn obj_f_vnt(
    f: &mut File,
    va: u16,
    vb: u16,
    vc: u16,
    na: u16,
    nb: u16,
    nc: u16,
    ta: u16,
    tb: u16,
    tc: u16,
) {
    let _ = write!(f, "f {va}/{na}/{ta} {vb}/{nb}/{tb} {vc}/{nc}/{tc}\n");
}

#[inline(always)]
fn obj_mtllib(f: &mut File, name: &str) {
    let _ = write!(f, "mtllib {name}\n");
}

#[inline(always)]
fn obj_usemtl(f: &mut File, name: &str) {
    let _ = write!(f, "usemtl {name}\n");
}

#[inline(always)]
fn mtl_newmtl(f: &mut File, name: &str) {
    let _ = write!(f, "newmtl {name}\n");
}

#[inline(always)]
fn mtl_map_kd(f: &mut File, name: &str) {
    let _ = write!(f, "map_Kd {name}\n");
}

#[inline(always)]
fn mtl_map_ka(f: &mut File, name: &str) {
    let _ = write!(f, "map_Ka {name}\n");
}

pub fn save_to_obj(mesh: &D3DFile, path: PathBuf) {
    let _ = std::fs::remove_file(&path);
    let mut f = std::fs::File::create(&path).unwrap();

    let mut already_saved = HashSet::new();

    let mut mtlpath = path.with_extension("mtl");
    let _ = std::fs::remove_file(&mtlpath);
    let mut mtl = std::fs::File::create(&mtlpath).unwrap();

    obj_mtllib(&mut f, "materials.mtl");
    obj_o(&mut f, "obj");

    /////////////////////////////////////////////////////////// POSITION
    let vertices = mesh
        .buffers
        .iter()
        .find(|x| x.r#type == "position")
        .unwrap();
    let vertices: Vec<_> = vertices.as_f32().chunks(3).collect();
    for v in vertices {
        obj_v(&mut f, v[0], v[1], v[2])
    }

    /////////////////////////////////////////////////////////// NORMAL
    let normals = mesh.buffers.iter().find(|x| x.r#type == "normal").unwrap();
    let vertices: Vec<_> = normals.as_f32().chunks(3).collect();
    for v in vertices {
        obj_vn(&mut f, v[0], v[1], v[2])
    }

    /////////////////////////////////////////////////////////// UVS
    let uvsa = mesh
        .buffers
        .iter()
        .filter(|x| x.r#type == "3")
        .nth(0)
        .unwrap();
    let uvsb = mesh
        .buffers
        .iter()
        .filter(|x| x.r#type == "3")
        .nth(1)
        .unwrap();
    let uvsa: Vec<_> = uvsa.as_f32().chunks(2).collect();
    let uvsb: Vec<_> = uvsb.as_f32().chunks(2).collect();
    for (a, b) in uvsa.iter().zip(uvsb.iter()) {
        obj_vt(&mut f, b[0], 1.0 - b[1]);
    }

    /////////////////////////////////////////////////////////// POLYGONS

    let indices = mesh.buffers.iter().find(|x| x.r#type == "index").unwrap();
    let indices = indices.as_u16();

    for (i, m) in mesh.meshes.iter().enumerate() {
        obj_g(&mut f, format!("group_{}", i).as_str());

        if let Some(map) = m.maps.get(1) {
            let matname = map.name.replace(".png", "");
            obj_usemtl(&mut f, &matname);

            if let None = already_saved.get(&matname) {
                mtl_newmtl(&mut mtl, &matname);
                mtl_map_kd(&mut mtl, &&map.name);
                already_saved.insert(matname);

                let _ = write!(&mut mtl, "\n");
            }
        } else {
            obj_usemtl(&mut f, "");
        }

        let start = m.index_start;
        let end = start + (m.tri_count * 3);
        let idxs = (start..end).step_by(3);
        for fi in idxs {
            obj_f_vnt(
                &mut f,
                indices[fi + 0] + 1,
                indices[fi + 1] + 1,
                indices[fi + 2] + 1,
                indices[fi + 0] + 1,
                indices[fi + 1] + 1,
                indices[fi + 2] + 1,
                indices[fi + 0] + 1,
                indices[fi + 1] + 1,
                indices[fi + 2] + 1,
            );
        }

        let _ = write!(f, "\n");
    }
}
