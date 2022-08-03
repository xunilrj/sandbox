use std::{
    collections::HashMap,
    io::Read,
    path::{Path, PathBuf},
    str::FromStr,
};

use bitflags::bitflags;
use bitvec::{order::Lsb0, slice::BitSlice};
use glam::{quat, Quat, Vec3};
use log::{debug, trace};

use crate::parser::{self, NomSlice};

#[derive(Debug)]
pub struct AnmFile {
    tracks: Vec<Track>,
}

impl AnmFile {
    pub fn new() -> Self {
        Self { tracks: vec![] }
    }

    pub fn push_animated_track(&mut self, bone: AnimatedTrack) {
        self.tracks.push(Track::Animated(bone));
    }
}

#[derive(Debug)]
pub struct AnimatedTrackFrame {
    time: f32,
    compressed: bool,
    rotation: [f64; 4],
    translation: [f64; 3],
}

impl AnimatedTrackFrame {
    pub fn translation_as_f32(&self) -> [f32; 3] {
        [
            self.translation[0] as f32,
            self.translation[1] as f32,
            self.translation[2] as f32,
        ]
    }

    pub fn rotation_as_f32(&self) -> [f32; 4] {
        [
            self.rotation[0] as f32,
            self.rotation[1] as f32,
            self.rotation[2] as f32,
            self.rotation[3] as f32,
        ]
    }
}

#[derive(Debug)]
pub struct AnimatedTrack {
    bone_id: u64,
    frames: Vec<AnimatedTrackFrame>,
}

impl AnimatedTrack {
    pub fn new() -> Self {
        Self {
            bone_id: 0,
            frames: vec![],
        }
    }
}

#[derive(Debug)]
pub enum Track {
    Animated(AnimatedTrack),
}

impl Track {
    pub fn as_animated(&self) -> &AnimatedTrack {
        match &self {
            Track::Animated(bone) => &bone,
        }
    }
}

pub struct TrackType {
    hash: u64,
    #[allow(dead_code)]
    a: u32,
    qty: u16,
    #[allow(dead_code)]
    b: u16,
}

fn skip_take_as_usize(bits: &BitSlice<Lsb0, u8>, skip: usize, take: usize) -> usize {
    let mut n = 0;
    let v: Vec<_> = bits.iter().skip(skip).take(take).rev().collect();
    // assert!(v.len() == take);
    for bit in v {
        n <<= 1;
        if bit == true {
            n |= 1;
        }
    }
    n
}

fn print_datapos(datapos: usize) {
    // println!(
    //     "\tdatapos: {} - {}th byte - {}th dword",
    //     datapos,
    //     datapos / 8,
    //     datapos / 32
    // );
}

bitflags! {
    struct BoneFlags: u32 {
        const DISABLED = 0x1;
        const TIME_BEHAVIOR = 0x2;
        const WEIGHT_BEHAVIOR = 0x4;
        const MOVER_ANIM = 0x10;
        const PROPERTY_ANIMATION = 0x20;
        const TEXTURE_MATRIX_ANIMATION = 0x40;
        const AUDIO_DATA_ANIMATION = 0x80;
        const DONT_OPTIMIZE = 0x100;
        const HOMOGENEOUS = 0x200;
        const MIXER_SCALED = 0x400;
        const MIXER_HOMOGENEOUS = 0x800;
        const STYLE_ANIMATION = 0x1000;
        const PROP_FORCE_UPDATE = 0x2000;
        const MIXER_OWNED = 0x4000;
        const MIXER_DIRTY = 0x8000;
        const ADDITIVE = 0x10000;
        const EXTERNALLY_OWNED = 0x20000;
        const DONT_MIX_PAUSED_CONTROLLERS = 0x40000;
        const RUNTIME_ANIMATION = 0x80000;
        const TRANSIENT_ANIMATION = 0x100000;
        const TOOL_ONLY = 0x200000;
        const KEYED_ATTACHMENT_ANIMATION = 0x400000;
        const MIXER_WEIGHTED_BLEND = 0x800000;
        const VALUE_KIND = 0xFF000000;
    }
}

impl From<u32> for BoneFlags {
    fn from(bits: u32) -> Self {
        BoneFlags::from_bits(bits).unwrap()
    }
}

pub fn convert(path: impl AsRef<Path>, mesh: String, skl: String, output: impl AsRef<Path>) {
    let path = path.as_ref();
    let filename = path.file_stem().unwrap().to_str().unwrap().to_string();

    let mut bar = progress::Bar::new();
    bar.set_job_title("Parsing...");

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

    let mut anm = AnmFile::new();

    let header = input.read_properties();

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u64("?");

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_f32("duration?");

    let _anim_length = input.parse_le_u32("animation length in bytes?");

    let qty_tracks = input.parse_le_u32("qty of tracks") as usize;
    let _length = input.parse_le_u32("length?");
    let qty_track_types = input.parse_le_u32("qty of track types");

    let mut track_types = vec![];
    for _ in 0..qty_track_types {
        let hash = input.parse_le_u64_with_debug("track type hash", |hash| {
            match hash {
                0x1019453EB19C1ABD => "Keyframed track of quaternion",
                0xCECACE3A835CB7EE => "Single Quaternion",
                0x5D3E9FC6FA9369BF => "Keyframed track of transform",
                0xF6F394AF6E4003AD => "Keyframed track of vector3",
                0xFC6597EB1FE5458E => "Compressed transforms",
                0xC1E84D6FF72CE80 => "Single Transform",
                _ => "?",
            }
            .to_string()
        });

        let hbytes = hash.to_le_bytes();
        let (hbytes, a) = parser::parse_le_u32(&hbytes[..]).unwrap();
        let (_, b) = parser::parse_le_u32(hbytes).unwrap();
        // println!("{} {}", a, b);
        let a = input.parse_le_u32("?");
        let qty = input.parse_le_u16("qty of tracks");
        let b = input.parse_le_u16("?");

        let i = TrackType { hash, qty, a, b };
        track_types.push(i);
    }

    let max_bounds = [
        0.00999999978,
        0.0500000007,
        0.100000001,
        0.5,
        0.699999988,
        0.800000012,
        1.0,
        1.5,
        2.0,
        3.0,
        4.0,
        5.5,
        7.0,
        8.5,
        10.0,
    ];

    let mut tracks = vec![];

    for track_type in track_types {
        // println!("buffer: {:?}", buffer.hash);
        match track_type.hash {
            //CompressedTransformKeys
            0xFC6597EB1FE5458E => {
                debug!("CompressedTransformKeys");
                for i in 0..track_type.qty {
                    debug!("Track: {}", i);
                    let mut track = AnimatedTrack::new();

                    let length = input.parse_n_bytes(1, "length");
                    let length = if length[0] == 255 {
                        input.parse_le_u16("length")
                    } else {
                        length[0] as u16
                    };

                    let header = input.parse_n_bytes(length as usize, "header");

                    if header.len() > 0 {
                        // 1111 1111 1111 1111 1111 1111 1111 1111/1111 1111 1111 1111 1111 1111 1111 1111
                        //                       |  sample count |                       |bps| mbi| bpb|
                        let (h, u32_0) = parser::parse_le_u32(header).unwrap();
                        let (_h, u32_1) = parser::parse_le_u32(h).unwrap();
                        let sample_count = (u32_0 & 0x3FFF) as usize;
                        let bits_per_sample = (((u32_1 >> 11) & 0x7) + 1) as usize;
                        let max_bounds_index = ((u32_1 >> 0x7) & 0xF) as usize;
                        let bits_per_bounds = ((u32_1 >> 0x3) & 0xF) as usize;

                        debug!("Sample Count: {}", sample_count);
                        debug!("Bit per Sample: {}", bits_per_sample);
                        debug!("Max Bounds Index: {}", max_bounds_index);
                        debug!("Bits pr Bounds: {}", bits_per_bounds);

                        let mut bounds_sizes = vec![];
                        let bits = BitSlice::<Lsb0, _>::from_slice(&header[..]).unwrap();
                        for i in 0..7 {
                            let pos = 3 * i + 0xE;
                            let v = skip_take_as_usize(bits, pos, 3);
                            bounds_sizes.push(v);
                        }

                        let max_bound = max_bounds[max_bounds_index];

                        let mut samples = 0;
                        let mut datapos = 50;

                        let mut time = 0.0;
                        loop {
                            if samples >= sample_count {
                                break;
                            }

                            let mut bounds_bitsizes = vec![];
                            for i in 0..7 {
                                let idx = skip_take_as_usize(bits, datapos, bounds_sizes[i]);
                                datapos += bounds_sizes[i];
                                bounds_bitsizes.push(idx);
                            }

                            let bitsizessum = bounds_bitsizes.iter().sum::<usize>();
                            let qty_samples = skip_take_as_usize(bits, datapos, bits_per_sample);
                            datapos += bits_per_sample;
                            samples += qty_samples;

                            let mut upper_value = 0.0;
                            let mut lower_value = 0.0;
                            if bitsizessum != 0 {
                                let b0 = skip_take_as_usize(bits, datapos, bits_per_bounds);
                                datapos += bits_per_bounds;

                                if bits_per_bounds == 0 {
                                    upper_value = 2.0 * max_bound;
                                    lower_value = -max_bound;
                                } else {
                                    let max = ((1 << bits_per_bounds) - 1);
                                    let new_b0 = b0 & max;
                                    let new_b0 = (new_b0 as f64) / (max as f64) * max_bound;

                                    upper_value = new_b0 * 2.0;
                                    lower_value = -new_b0;
                                }
                            }

                            for _ in 0..qty_samples {
                                let mut samples = vec![];
                                let mut fsamples = vec![];
                                for &size in &bounds_bitsizes {
                                    //see sub_6c53f0

                                    let v = skip_take_as_usize(bits, datapos, size);
                                    datapos += size;

                                    samples.push(v);

                                    let v = if size == 0 {
                                        0.0
                                    } else {
                                        let max = (1 << size) - 1;
                                        let v = v & max;
                                        let v = (v as f64) / (max as f64);
                                        v * upper_value + lower_value
                                    };

                                    fsamples.push(v);
                                }

                                let frame = AnimatedTrackFrame {
                                    time,
                                    compressed: true,
                                    rotation: [fsamples[0], fsamples[1], fsamples[2], fsamples[3]],
                                    translation: [fsamples[4], fsamples[5], fsamples[6]],
                                };
                                debug!("{:?}", frame);

                                track.frames.push(frame);
                            }

                            if samples < sample_count {
                                let _ = skip_take_as_usize(bits, datapos, 1);
                                datapos += 1;
                            } else {
                                break;
                            }

                            time += 1.0 / 30.0;
                        }

                        let all_padding = bits.iter().skip(datapos).all(|x| x == false);

                        assert!(samples == sample_count);
                        if !all_padding {
                            assert!(
                                datapos == header.len() * 8,
                                "{} {} remaining {:?}",
                                datapos,
                                header.len() * 8,
                                bits.iter()
                                    .skip(datapos)
                                    .map(|x| x == true)
                                    .collect::<Vec<_>>()
                            );
                        }
                    }

                    let data = input.parse_length1_buffer("?");

                    let bits = BitSlice::<Lsb0, _>::from_slice(&data[..]).unwrap();

                    //         00000111|11|100|0|0000|100|000

                    // 1111011011000111|00|100|0|0101|100|010
                    //                 |c |tbl| |    | b | a

                    // 0x74A70B mov edx,[ebp], EDX=1, SS=None, EBP=180736300, mem[180736300]=U32(4140245346)
                    // 0x74A70E and edx,7, EDX=4140245346
                    // 0x74A711 lea edi,[eax+edx], EDI=1, EAX=14, EDX=2
                    let a = skip_take_as_usize(&bits, 0, 3);
                    let b = skip_take_as_usize(&bits, 3, 3) + 1;
                    let b2 = skip_take_as_usize(&bits, 6, 4);
                    let tblA = skip_take_as_usize(&bits, 11, 3);
                    let c = skip_take_as_usize(&bits, 14, 2);
                    let d = skip_take_as_usize(&bits, 16, 5);
                    let e = skip_take_as_usize(&bits, 21, 11);

                    debug!("Second Buffer Header: {:?}", (a, b, b2, tblA, c, d, e));

                    // table A
                    // 00817b80  float data_817b80 = 0.100000001
                    // 00817b84  float data_817b84 = 0.166666672
                    // 00817b88  float data_817b88 = 0.333333343
                    // 00817b8c  float data_817b8c = 0.5
                    // 00817b90  float data_817b90 = 0.666666687
                    // 00817b94  float data_817b94 = 1
                    // 00817b98  float data_817b98 = 2

                    if tblA == 7 {
                    } else {
                        //look up at table
                    }

                    // println!(
                    //     "a: {}, b: {}, b2: {}, tblA: {}, c: {}, d: {}, e: {}",
                    //     a, b, b2, tblA, c, d, e
                    // );

                    tracks.push(track);
                }
            }

            //compressedkeys<quaternion>
            0x9A946F2A83FC7658 => {
                todo!();
                // for _ in 0..track_type.qty {
                //     let mut bone = AnimatedTrack::new();
                //     bone.frames.push(AnimatedTrackFrame {
                //         compressed: false,
                //         rotation: [0.0, 0.0, 0.0, 1.0],
                //         translation: [0.0, 0.0, 0.0],
                //     });
                //     tracks.push(bone);
                // }
            }

            //SngleValue<vector3>
            0x6B77C806C0E23EA1 => {
                debug!("SingleValue<Vector3>");
                for _ in 0..track_type.qty {
                    let mut bone = AnimatedTrack::new();
                    let t = input.read_vec3("vec3");
                    bone.frames.push(AnimatedTrackFrame {
                        time: 0.0,
                        compressed: false,
                        rotation: [0.0, 0.0, 0.0, 1.0],
                        translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                    });
                    tracks.push(bone);
                }
            }

            // SingleValue<Quaternion>
            0xCECACE3A835CB7EE => {
                debug!("SingleValue<Quaternion>");
                for _ in 0..track_type.qty {
                    let mut bone = AnimatedTrack::new();
                    let q = input.read_quat("quat");
                    bone.frames.push(AnimatedTrackFrame {
                        time: 0.0,
                        compressed: false,
                        rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                        translation: [0.0, 0.0, 0.0],
                    });
                    tracks.push(bone);
                }
            }

            // SingleValue<Transform>
            0xC1E84D6FF72CE80 => {
                debug!("SingleValue<Transform>");
                for _ in 0..track_type.qty {
                    let mut bone = AnimatedTrack::new();
                    let q = input.read_quat("quat");
                    let t = input.read_vec3("vec3");
                    bone.frames.push(AnimatedTrackFrame {
                        time: 0.0,
                        compressed: false,
                        rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                        translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                    });
                    tracks.push(bone);
                }
            }

            // keyframedvalue<quaternion>
            0x1019453EB19C1ABD => {
                debug!("keyframedvalue<quaternion>");

                for _ in 0..track_type.qty {
                    log::debug!("New Track - keyframed quaternions");

                    let mut track = AnimatedTrack::new();

                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");
                    let bone_id = input.parse_le_u64("bone id");
                    let _ = input.parse_le_u32("min");
                    let _ = input.parse_le_u32("max");
                    let _ = input.read_quat("?");
                    let _ = input.read_quat("?");

                    let _ = input.parse_le_u32("?");

                    let qty = input.parse_le_u32("qty keyframes");

                    let t = input.parse_le_u32("?");
                    let _ = input.parse_n_bytes(1, "?");

                    let mut time = 0.0;
                    for i in 0..qty {
                        log::debug!("keyframe quaternion");
                        let t = input.parse_le_u32("type");

                        match t {
                            2 => {
                                let q = input.read_quat("");

                                track.frames.push(AnimatedTrackFrame {
                                    time,
                                    compressed: false,
                                    rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                                    translation: [0.0, 0.0, 0.0],
                                });

                                if i == (qty - 1) {
                                    break;
                                }
                            }
                            x => todo!("{}", x),
                        }

                        let _ = input.parse_n_bytes(1, "?");

                        time += 1.0 / 30.0;
                    }

                    track.bone_id = bone_id;
                    tracks.push(track);
                }
            }

            // keyframedvalue<transform>
            0x5D3E9FC6FA9369BF => {
                debug!("keyframedvalue<transform>");

                for _ in 0..track_type.qty {
                    let mut bone = AnimatedTrack::new();

                    log::debug!("- new bone transform");
                    let header_length = input.parse_le_u32("header length");
                    assert!(header_length == 24);
                    let subheader_length = input.parse_le_u32("subheader length");
                    assert!(subheader_length == 20);

                    let boneid = input.parse_le_u64("boneid");
                    let _ = input.parse_le_u32("?");

                    let _ = input.parse_flag_le_u32::<BoneFlags>("flags?");

                    let (minq, mint) = input.read_length_transform("min");
                    let (maxq, maxt) = input.read_length_transform("max");

                    let _ = input.parse_le_u32("frames length in bytes");
                    let qty = input.parse_le_u32("qty");

                    let mut time = 0.0;
                    for i in 0..=qty {
                        log::debug!("- frame {}", i);
                        let t = input.parse_le_u32("type");

                        match t {
                            0 => {}
                            2 => {
                                let (q, t) = input.read_length_transform("max");

                                bone.frames.push(AnimatedTrackFrame {
                                    time,
                                    compressed: false,
                                    rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                                    translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                                });

                                if i == qty {
                                    break;
                                }

                                let _ = input.parse_le_f32("time");
                            }
                            x => todo!("{}", x),
                        }

                        let v = input.parse_n_bytes(1, "?");
                        debug!("v: {:?}", v);

                        time += 1.0 / 30.0;
                    }

                    bone.bone_id = boneid;
                    tracks.push(bone);
                }
            }

            //keyframedvalue<vector3>
            0xF6F394AF6E4003AD => {
                debug!("keyframedvalue<vector3>");

                for _ in 0..track_type.qty {
                    let mut track = AnimatedTrack::new();
                    log::debug!("- new bone vec3");

                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");

                    let _ = input.read_vec3("min");
                    let _ = input.read_vec3("max");

                    let _ = input.parse_le_u32("?");
                    let qty = input.parse_le_u32("qty frames");

                    let mut time = 0.0;
                    for i in 0..qty {
                        log::debug!("-- new vec3");
                        let t = input.parse_le_u32("vector type");

                        match t {
                            0 => {
                                track.frames.push(AnimatedTrackFrame {
                                    time,
                                    compressed: false,
                                    rotation: [0.0, 0.0, 0.0, 1.0],
                                    translation: [0.0, 0.0, 0.0],
                                });
                            }
                            2 => {
                                let _ = input.parse_le_u32("?");
                                let t = input.read_vec3("?");

                                track.frames.push(AnimatedTrackFrame {
                                    time,
                                    compressed: false,
                                    rotation: [0.0, 0.0, 0.0, 1.0],
                                    translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                                });
                            }
                            x => todo!("{}", x),
                        }

                        let _ = input.parse_n_bytes(1, "?");

                        time += 1.0 / 30.0;
                    }

                    let _ = input.parse_le_u32("?");
                    let t = input.read_vec3("?");
                    track.frames.push(AnimatedTrackFrame {
                        time,
                        compressed: false,
                        rotation: [0.0, 0.0, 0.0, 1.0],
                        translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                    });

                    tracks.push(track);
                }
            }

            _ => todo!("{:X?}", track_type.hash),
        }
    }

    debug!("One Value per track");
    for i in 0..qty_tracks {
        let _ = input.parse_flag_le_u32::<BoneFlags>(format!("value: {}", i).as_str());
    }

    let _ = input.parse_le_u16("?");

    assert!(
        qty_tracks == tracks.len(),
        "{} == {}",
        qty_tracks,
        tracks.len()
    );

    debug!("Bone IDs");
    for i in 0..qty_tracks {
        let k = input.parse_le_u64("Bone ID");
        let _ = input.parse_le_u32("");
        if tracks[i].bone_id == 0 || tracks[i].bone_id == k {
            tracks[i].bone_id = k;
        } else {
            panic!("Old value: {}, new value: {}", tracks[i].bone_id, k);
        }
    }

    let mut bone_id_to_idx = HashMap::new();
    let mut bone_idx_to_id = HashMap::new();
    for (i, bone) in tracks.iter().enumerate() {
        bone_id_to_idx.insert(bone.bone_id, i);
        bone_idx_to_id.insert(i, bone.bone_id);
    }

    let skl = PathBuf::from_str(&skl).unwrap();
    let skl = crate::skl::parse_skl(&skl);
    let mut gltf = crate::skl::gltf::to_gltf(&skl);

    let bones_ids = gltf["skins"][0]["joints"]
        .members()
        .map(|x| {
            gltf["nodes"][x.as_usize().unwrap()]["name"]
                .as_str()
                .unwrap()
                .parse::<u64>()
                .unwrap()
        })
        .collect::<Vec<_>>();

    let d3dmesh = PathBuf::from_str(&mesh).unwrap();
    let d3dmesh = crate::d3dmesh::parse_d3dmesh(&d3dmesh, Some(&skl), None).unwrap();
    crate::d3dmesh::outputs::gltf::add_mesh_to_gltf(&d3dmesh, &mut gltf, false, Some(0));

    //time buffer

    let mut time_buffer: Vec<u8> = vec![];
    let mut time_bone_id_to_offset = HashMap::new();
    for bone_id in bones_ids.iter() {
        let track = &tracks[bone_id_to_idx[bone_id]];
        assert!(track.bone_id == *bone_id);

        time_bone_id_to_offset.insert(*bone_id, time_buffer.len());
        for frame in track.frames.iter() {
            time_buffer.extend(frame.time.to_le_bytes());
        }
    }

    //transformations buffer
    let mut rotation_buffer: Vec<u8> = vec![];
    let mut translation_buffer: Vec<u8> = vec![];

    let mut r_bone_id_to_offset = HashMap::new();
    let mut t_bone_id_to_offset = HashMap::new();
    for bone_id in bones_ids.iter() {
        let track = &tracks[bone_id_to_idx[bone_id]];
        assert!(track.bone_id == *bone_id);

        let b: Vec<u8> = track
            .frames
            .iter()
            .scan(Quat::default(), |acc, frame| {
                // if frame.compressed {
                //     *acc = glam::Quat::from_array(frame.rotation_as_f32());
                //     let q = acc.normalize();
                //     Some([q.x, q.y, q.z, q.w])
                // } else {
                Some(frame.rotation_as_f32())
                // }
            })
            .flat_map(|x| x)
            .flat_map(|x| x.to_le_bytes())
            .collect();
        r_bone_id_to_offset.insert(*bone_id, rotation_buffer.len());
        rotation_buffer.extend(b);

        let b: Vec<u8> = track
            .frames
            .iter()
            .scan(glam::Vec3::default(), |acc, frame| {
                // use glam::Vec3Swizzles;
                // let t = glam::Vec3::from_slice(&frame.translation_as_f32()[..]).xzy() * 0.1; // why 0.1 here?
                // *acc = *acc + t;
                // Some(*acc.as_ref())
                Some(frame.translation_as_f32())
            })
            .flat_map(|x| x)
            .flat_map(|x| x.to_le_bytes())
            .collect();
        t_bone_id_to_offset.insert(*bone_id, translation_buffer.len());
        translation_buffer.extend(b);
    }

    let mut time_buffer_view = crate::gltf::push_buffer(&mut gltf, time_buffer.as_slice());
    time_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let time_buffer_idx = crate::gltf::push_buffer_view(&mut gltf, time_buffer_view);

    let mut rot_buffer_view = crate::gltf::push_buffer(&mut gltf, rotation_buffer.as_slice());
    rot_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let rot_buffer_idx = crate::gltf::push_buffer_view(&mut gltf, rot_buffer_view);

    let mut t_buffer_view = crate::gltf::push_buffer(&mut gltf, translation_buffer.as_slice());
    t_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let translation_buffer_idx = crate::gltf::push_buffer_view(&mut gltf, t_buffer_view);

    let mut anim01 = json::object! {
        name: filename
    };

    for bone_id in bones_ids.iter() {
        let track = &tracks[bone_id_to_idx[bone_id]];
        assert!(track.bone_id == *bone_id);

        let bone_name = format!("{}", bone_id);

        if let Some(node_id) = gltf["nodes"]
            .members()
            .position(|node| node["name"] == bone_name)
        {
            let time_acessor_idx = crate::gltf::push_accessor(
                &mut gltf,
                json::object! {
                    bufferView: time_buffer_idx,
                    componentType: 5126,
                    count: track.frames.len(),
                    type: "SCALAR",
                    byteOffset: time_bone_id_to_offset[&bone_id],
                },
            );
            let rot_acessor_idx = crate::gltf::push_accessor(
                &mut gltf,
                json::object! {
                    bufferView : rot_buffer_idx,
                    componentType : 5126,
                    count : track.frames.len(),
                    type : "VEC4",
                    byteOffset: r_bone_id_to_offset[&bone_id],
                },
            );
            let translation_acessor_idx = crate::gltf::push_accessor(
                &mut gltf,
                json::object! {
                    bufferView: translation_buffer_idx,
                    componentType: 5126,
                    count: track.frames.len(),
                    type: "VEC3",
                    byteOffset: t_bone_id_to_offset[&bone_id],
                },
            );

            let rot_sampler_idx = crate::gltf::push_sampler(
                &mut anim01,
                json::object! {
                    input : time_acessor_idx,
                    interpolation : "LINEAR",
                    output : rot_acessor_idx
                },
            );
            crate::gltf::push_channel(
                &mut anim01,
                json::object! {
                    sampler: rot_sampler_idx,
                    target: json::object!{
                        node: node_id,
                        path: "rotation"
                    }
                },
            );

            // skli == 0 translation gives root motion
            // if skli == 0 {
            let translation_sampler_idx = crate::gltf::push_sampler(
                &mut anim01,
                json::object! {
                    input: time_acessor_idx,
                    interpolation: "LINEAR",
                    output: translation_acessor_idx
                },
            );
            crate::gltf::push_channel(
                &mut anim01,
                json::object! {
                    sampler: translation_sampler_idx,
                    target: json::object!{
                        node: node_id,
                        path: "translation"
                    }
                },
            );
            // }
        }
    }

    gltf["animations"] = json::array![anim01];

    crate::skl::gltf::save(output, &gltf);
}
