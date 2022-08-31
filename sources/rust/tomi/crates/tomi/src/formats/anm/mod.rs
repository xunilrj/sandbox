pub mod gltf;

use std::path::Path;

use bitvec::order::Lsb0;
use bitvec::slice::BitSlice;
use glam::{quat, vec3, Quat, Vec3};
use iced_x86::Register;
use log::debug;
use x86_emulator::Value;

use crate::{
    checksum_mapping::ChecksumMap,
    parser::{parse_le_u32, NomSlice},
    utils::{read_to_end, IOError},
};

#[derive(Debug)]
pub struct Frame {
    pub time: f32,
    pub rotation: Quat,
    pub translation: Vec3,
}

#[derive(Debug)]
pub struct Track {
    pub bone_name_hash: u64,
    pub bone_name: Option<String>,
    pub frames: Vec<Frame>,
}

// pub struct PoseTransforms {
//     pub local_translation: Vec3,
//     pub local_rotation: Quat,
//     pub pose_matrix: Mat4,
//     pub bone_length: f32,
// }

// pub struct Pose {
//     pub bones: HashMap<String, PoseTransforms>,
// }

#[derive(Debug)]
pub struct Animation {
    pub name: String,
    pub tracks: Vec<Track>,
}

impl Animation {
    // pub fn pose_at(&self, skl: &Skeleton, time: f32) -> Pose {
    //     let mut bones = HashMap::new();

    //     for b in skl.bones.iter() {
    //         let parent_global = skl.bones[b.index]
    //             .parent_index
    //             .and_then(|p| skl.bones[p].bind_pose_transformation.clone())
    //             .unwrap_or(Mat4::IDENTITY);

    //         let name = b
    //             .name
    //             .as_ref()
    //             .map(|x| x.clone())
    //             .unwrap_or_else(|| b.name_hash.to_string());
    //         let (local_translation, local_rotation) = self.tracks[b.index]
    //             .frames
    //             .first()
    //             .map(|x| (x.translation.clone(), x.rotation.clone()))
    //             .unwrap_or_else(|| (Vec3::ZERO, Quat::IDENTITY));

    //         let pose_matrix = parent_global
    //             * Mat4::from_scale_rotation_translation(
    //                 vec3(1.0, 1.0, 1.0),
    //                 local_rotation.clone(),
    //                 local_translation.clone(),
    //             );

    //         let (_, _, parent_pos) = parent_global.to_scale_rotation_translation();
    //         let (_, _, pos) = pose_matrix.to_scale_rotation_translation();
    //         let bone_length = parent_pos.distance(pos);
    //         bones.insert(
    //             name,
    //             PoseTransforms {
    //                 local_translation,
    //                 local_rotation,
    //                 pose_matrix,
    //                 bone_length,
    //             },
    //         );
    //     }

    //     Pose { bones }
    // }
}

#[derive(Debug)]
pub enum AnmParserError {
    IO(IOError),
    InvalidMagicNumber,
}

pub struct AnmParser {}

impl AnmParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(
        &mut self,
        path: impl AsRef<Path>,
        mapping: &ChecksumMap,
    ) -> Result<Animation, AnmParserError> {
        let path = path.as_ref();

        let bytes = read_to_end(&path).map_err(AnmParserError::IO)?;
        let mut input = NomSlice::new(bytes.as_slice());

        if !input.read_ertm_magic_number() {
            return Err(AnmParserError::InvalidMagicNumber);
        }

        let _ = input.read_properties(&mapping);
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");
        let _ = input
            .parse_le_u64_with_debug("?", |k| mapping.get_mapping(k).unwrap_or("?".to_string()));

        let _ = input.parse_le_u32("?");
        let animation_duration = input.parse_le_f32("duration?");

        let _anim_length = input.parse_le_u32("animation length in bytes?");

        let qty_tracks = input.parse_le_u32("qty of tracks") as usize;
        let _length = input.parse_le_u32("length?");
        let qty_track_types = input.parse_le_u32("qty of track types");

        pub struct TrackType {
            hash: u64,
            #[allow(dead_code)]
            a: u32,
            qty: u16,
            #[allow(dead_code)]
            b: u16,
        }
        let mut track_types = vec![];
        for _ in 0..qty_track_types {
            let hash = input.parse_le_u64_with_debug("track type hash", |k| {
                mapping.get_mapping(k).unwrap_or("?".to_string())
            });

            let a = input.parse_le_u32("?");
            let qty = input.parse_le_u16("qty of tracks");
            let b = input.parse_le_u16("?");

            let i = TrackType { hash, qty, a, b };
            track_types.push(i);
        }

        assert!(track_types.iter().map(|x| x.qty as usize).sum::<usize>() as usize == qty_tracks);

        let mut tracks = vec![];

        for track_type in track_types {
            match track_type.hash {
                0x1019453EB19C1ABD => {
                    parse_keyframedvalue_quaternion(
                        track_type.qty as usize,
                        &mut input,
                        mapping,
                        &mut tracks,
                    );
                }
                0x5D3E9FC6FA9369BF => {
                    parse_keyframedvalue_transform(
                        track_type.qty as usize,
                        &mut input,
                        mapping,
                        animation_duration,
                        &mut tracks,
                    );
                }
                0xF6F394AF6E4003AD => {
                    parse_keyframedvalue_vector3(
                        track_type.qty as usize,
                        &mut input,
                        mapping,
                        &mut tracks,
                    );
                }
                //CompressedTransformKeys
                0xFC6597EB1FE5458E => {
                    debug!("CompressedTransformKeys");

                    // let code = x86_emulator::code_from_hex_string(include_str!("compressed.txt"));
                    // // x86_emulator::disassembly(&code);

                    // let fn0074a990 =
                    //     x86_emulator::code_from_hex_string(include_str!("0x0074a990.txt"));
                    // let mut ctx = x86_emulator::Context::new();
                    // ctx.debug = true;

                    // let mut data = vec![];
                    // data.extend(0.0f32.to_le_bytes());
                    // data.extend(0.0f32.to_le_bytes());
                    // data.extend(0.0f32.to_le_bytes());
                    // data.extend(1.0f32.to_le_bytes());
                    // data.extend(0.0f32.to_le_bytes());
                    // data.extend(0.0f32.to_le_bytes());
                    // data.extend(0.0f32.to_le_bytes());
                    // ctx.mount_mem(0x895184, data);

                    // ctx.set_register(Register::EAX, 0x006c65f0);
                    // ctx.set_register(Register::EBX, 0x03e3a5a0);
                    // ctx.set_register(Register::ECX, 0x37dfd68);
                    // ctx.set_register(Register::EDI, 0x038d9038);

                    // let mut data = vec![];
                    // data.extend(0x007ef8f8u32.to_le_bytes());
                    // data.extend(0x02fdc328u32.to_le_bytes());
                    // data.extend(0x765ba466u32.to_le_bytes());
                    // data.extend(0x632fd7a5u32.to_le_bytes());
                    // data.extend(0x03000200u32.to_le_bytes()); // 0x10
                    // data.extend(0x00050004u32.to_le_bytes()); // 4
                    // data.extend(0x037e3dfcu32.to_le_bytes()); // 8
                    // data.extend(0x00000060u32.to_le_bytes()); // c
                    // data.extend(0x00000015u32.to_le_bytes()); // 0x20
                    // data.extend(0x00000000u32.to_le_bytes()); // 4
                    // data.extend(0x00000000u32.to_le_bytes()); // 8
                    // data.extend(0x00000000u32.to_le_bytes()); // c
                    // data.extend(0x3d088889u32.to_le_bytes()); // 0x30
                    // data.extend(0x00000000u32.to_le_bytes());
                    // data.extend(0x00000000u32.to_le_bytes());
                    // data.extend(0x00015100u32.to_le_bytes());
                    // data.extend(0x037e3d98u32.to_le_bytes()); // 0x40
                    // data.extend(0x00000318u32.to_le_bytes());
                    // data.extend(0x00000031u32.to_le_bytes());
                    // data.extend(0xb5743900u32.to_le_bytes());
                    // data.extend(0x00000022u32.to_le_bytes()); // 0x50

                    // ctx.mount_mem(0x37dfd68, data);

                    // ctx.mount_mem(0x006c65f0, code);
                    // ctx.mount_mem(0x0074a990, fn0074a990);
                    // ctx.set_register(Register::EIP, 0x006c65f0);

                    // ctx.mount_mem(0x00c6f5bc - 0x00010000, vec![0u8; 0x00020000]);

                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x0), Value::U32(0x004a35af));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x4), Value::U32(0x038d9038));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x8), Value::U32(0x00c6f60c));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0xC), Value::U32(0x3e3ddcf2));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x10), Value::U32(0x3f3ddd00));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x14), Value::U32(0x00000000));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x18), Value::U32(0x03e3e840));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x1C), Value::U32(0x038d8fcc));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x20), Value::U32(0x00000000));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x24), Value::U32(0x00000000));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x28), Value::U32(0x00000000));
                    // ctx.set_value(Value::Memory(0x00c6f5bc + 0x2C), Value::U32(0x00000000));
                    // ctx.set_register(Register::ESP, 0x00c6f5bc);

                    // ctx.break_if_eip(0x006c6fd2);
                    // x86_emulator::run(&mut ctx, 100);
                    // todo!();

                    for i in 0..track_type.qty {
                        debug!("Track: {}", i);

                        let mut frames = vec![];

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
                            let (h, u32_0) = parse_le_u32(header).unwrap();
                            let (_h, u32_1) = parse_le_u32(h).unwrap();
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
                            let max_bound = max_bounds[max_bounds_index];
                            debug!("max bound: {}", max_bound);

                            let mut sample_idx = 0;
                            let mut datapos = 50;

                            let mut time = 0.0;
                            // see 0x006c65f0 @ 0x6C66F9
                            let mut accum_rot = Quat::IDENTITY;
                            let mut accum_translation = Vec3::ZERO;

                            let mut sum_last = false;
                            let mut last_rot = Quat::IDENTITY;
                            let mut last_translation = Vec3::ZERO;

                            loop {
                                if sample_idx >= sample_count {
                                    break;
                                }

                                let mut bounds_bitsizes = vec![];
                                for i in 0..7 {
                                    let idx = skip_take_as_usize(bits, datapos, bounds_sizes[i]);
                                    datapos += bounds_sizes[i];
                                    bounds_bitsizes.push(idx);
                                }

                                let bitsizessum = bounds_bitsizes.iter().sum::<usize>();
                                let qty_samples =
                                    skip_take_as_usize(bits, datapos, bits_per_sample);
                                datapos += bits_per_sample;
                                debug!("qty_samples: {}", qty_samples);

                                let mut upper_value = 0.0;
                                let mut lower_value = 0.0;
                                if bitsizessum != 0 {
                                    let b0 = skip_take_as_usize(bits, datapos, bits_per_bounds);
                                    datapos += bits_per_bounds;

                                    if bits_per_bounds == 0 {
                                        upper_value = 2.0 * max_bound;
                                        lower_value = -max_bound;
                                    } else {
                                        let max = (1 << bits_per_bounds) - 1;
                                        let new_b0 = b0; // & max;
                                        let new_b0 = (new_b0 as f64) / (max as f64) * max_bound;
                                        upper_value = new_b0 * 2.0;
                                        lower_value = -new_b0;
                                    }
                                }

                                for _ in 0..qty_samples {
                                    let mut sizes = vec![];
                                    let mut samples = vec![];
                                    let mut fsamples = vec![];
                                    for &size in &bounds_bitsizes {
                                        //see sub_6c6460 for whole tranform
                                        //see sub_6c53f0 for quat
                                        //see sub_6c4250 for vec3
                                        let v = skip_take_as_usize(bits, datapos, size);
                                        datapos += size;
                                        sizes.push(size);

                                        samples.push(v);

                                        if v == 0 {
                                            fsamples.push(0.0);
                                        } else {
                                            if size == 0 {
                                                fsamples.push(0.0);
                                            } else {
                                                let max = (1 << size) - 1;
                                                let v = v; // & max;
                                                if v == max {
                                                    fsamples.push(max_bound);
                                                } else {
                                                    let new_v = (v as f64) / (max as f64);
                                                    // debug!(
                                                    //     "{} * {} + {}",
                                                    //     new_v, upper_value, lower_value
                                                    // );
                                                    fsamples
                                                        .push(new_v * upper_value + lower_value);
                                                }
                                            };
                                        }
                                    }

                                    // debug!("sizes: {:?}", sizes);
                                    // debug!("samples: {:?}", samples);
                                    // debug!("fsamples: {:?}", fsamples);

                                    let mut delta = quat(
                                        fsamples[0] as f32,
                                        fsamples[1] as f32,
                                        fsamples[2] as f32,
                                        fsamples[3] as f32,
                                    );
                                    if sum_last {
                                        debug!(
                                            "sum last: {} {} => {}",
                                            delta.x,
                                            last_rot.x,
                                            delta.x + last_rot.x
                                        );
                                        debug!(
                                            "sum last: {} {} => {}",
                                            delta.y,
                                            last_rot.y,
                                            delta.y + last_rot.y
                                        );
                                        debug!(
                                            "sum last: {} {} => {}",
                                            delta.z,
                                            last_rot.z,
                                            delta.z + last_rot.z
                                        );
                                        debug!(
                                            "sum last: {} {} => {}",
                                            delta.w,
                                            last_rot.w,
                                            delta.w + last_rot.w
                                        );
                                        delta = delta + last_rot;
                                    }
                                    last_rot = delta;
                                    let next_rot = (accum_rot + delta).normalize();
                                    debug!("{} + {} => {}", accum_rot.x, delta.x, next_rot.x);
                                    debug!("{} + {} => {}", accum_rot.y, delta.y, next_rot.y);
                                    debug!("{} + {} => {}", accum_rot.z, delta.z, next_rot.z);
                                    debug!("{} + {} => {}", accum_rot.w, delta.w, next_rot.w);
                                    accum_rot = next_rot;

                                    let mut delta = vec3(
                                        fsamples[4] as f32,
                                        fsamples[5] as f32,
                                        fsamples[6] as f32,
                                    );
                                    if sum_last {
                                        debug!(
                                            "sum last: {} {} => {}",
                                            delta.x,
                                            last_translation.x,
                                            delta.x + last_translation.x
                                        );
                                        debug!(
                                            "sum last: {} {} => {}",
                                            delta.y,
                                            last_translation.y,
                                            delta.y + last_translation.y
                                        );
                                        debug!(
                                            "sum last: {} {} => {}",
                                            delta.z,
                                            last_translation.z,
                                            delta.z + last_translation.z
                                        );
                                        delta = delta + last_translation;
                                    }
                                    last_translation = delta;
                                    let next_translation = accum_translation + delta;
                                    debug!(
                                        "{} + {} => {}",
                                        accum_translation.x, delta.x, next_translation.x
                                    );
                                    debug!(
                                        "{} + {} => {}",
                                        accum_translation.y, delta.y, next_translation.y
                                    );
                                    debug!(
                                        "{} + {} => {}",
                                        accum_translation.z, delta.z, next_translation.z
                                    );
                                    accum_translation = next_translation;

                                    // Order of samples see
                                    // 006c64b9
                                    let frame = Frame {
                                        time,
                                        rotation: accum_rot.clone(),
                                        translation: accum_translation,
                                    };
                                    // see 0x6C674B for cmp sample_idx and sample_count
                                    debug!("{}/{} {:?}", sample_idx, sample_count, frame);
                                    debug!("fsamples: {:?}", fsamples);
                                    debug!("last r: {}", last_rot);
                                    debug!("last t: {}", last_translation);

                                    frames.push(frame);
                                    sample_idx += 1;
                                    time += 1.0 / 30.0;
                                }

                                if sample_idx < sample_count {
                                    let v = skip_take_as_usize(bits, datapos, 1);
                                    println!("SomeValue: {v}");
                                    // this is used at 0x6C67C7
                                    if v == 1 {
                                        sum_last = true;
                                    }
                                    datapos += 1;
                                } else {
                                    break;
                                }
                            }

                            let all_padding = bits.iter().skip(datapos).all(|x| x == false);

                            assert!(sample_idx == sample_count);
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
                        debug!("Second buffer length: {}", data.len());

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

                        tracks.push(Track {
                            bone_name_hash: 0,
                            bone_name: None,
                            frames,
                        });
                    }
                }

                // SingleValue<Vector3>
                0x6B77C806C0E23EA1 => {
                    debug!("SingleValue<Vector3>");
                    for _ in 0..track_type.qty {
                        let translation = input.read_vec3("vec3");
                        tracks.push(Track {
                            bone_name_hash: 0,
                            bone_name: None,
                            frames: vec![Frame {
                                time: 0.0,
                                rotation: Quat::IDENTITY,
                                translation,
                            }],
                        });
                    }
                }

                // SingleValue<Quaternion>
                0xCECACE3A835CB7EE => {
                    debug!("SingleValue<Quaternion>");
                    for _ in 0..track_type.qty {
                        let rotation = input.read_quat("quat");
                        tracks.push(Track {
                            bone_name_hash: 0,
                            bone_name: None,
                            frames: vec![Frame {
                                time: 0.0,
                                rotation,
                                translation: Vec3::ZERO,
                            }],
                        });
                    }
                }

                // SingleValue<Transform>
                0xC1E84D6FF72CE80 => {
                    debug!("SingleValue<Transform>");
                    for _ in 0..track_type.qty {
                        let rotation = input.read_quat("quat");
                        let translation = input.read_vec3("vec3");
                        tracks.push(Track {
                            bone_name_hash: 0,
                            bone_name: None,
                            frames: vec![Frame {
                                time: 0.0,
                                rotation,
                                translation,
                            }],
                        });
                    }
                }

                hash => todo!("Track type {} {:?}", hash, mapping.get_mapping(hash)),
            }
        }

        for _ in 0..qty_tracks {
            let _ = input.parse_le_u32("?");
        }

        let _ = input.parse_le_u16("?");

        assert!(
            qty_tracks == tracks.len(),
            "{} == {}",
            qty_tracks,
            tracks.len()
        );

        for i in 0..qty_tracks {
            let bone_name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
                mapping.get_mapping(k).unwrap_or("?".to_string())
            });
            let name = mapping.get_mapping(bone_name_hash);
            let _ = input.parse_le_u32("");

            tracks[i].bone_name_hash = bone_name_hash;
            tracks[i].bone_name = name;
        }

        input.assert_eof();

        for t in tracks.iter() {
            assert!(t.bone_name_hash != 0);
        }

        let name = path.file_stem().unwrap().to_str().unwrap().to_string();
        Ok(Animation { name, tracks })
    }
}

fn parse_keyframedvalue_vector3(
    qty: usize,
    input: &mut NomSlice,
    mapping: &ChecksumMap,
    tracks: &mut Vec<Track>,
) {
    debug!("Track type: keyframedvalue<vector3>");
    for _ in 0..qty {
        let mut track = Track {
            bone_name_hash: 0,
            bone_name: None,
            frames: vec![],
        };

        debug!("New Track - keyframedvalue<vector3>");
        let header_length = input.parse_le_u32("Header length");
        assert!(header_length == 24);
        let subheader_length = input.parse_le_u32("Subheader length");
        assert!(subheader_length == 20);

        let bone_name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
            mapping.get_mapping(k).unwrap_or("?".to_string())
        });
        track.bone_name_hash = bone_name_hash;
        track.bone_name = mapping.get_mapping(bone_name_hash);

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");

        let _ = input.read_vec3("min");
        let _ = input.read_vec3("max");

        let _ = input.parse_le_u32("?");
        let qty = input.parse_le_u32("qty frames");

        let mut time = 0.0;
        for i in 0..qty {
            debug!("New Keyframe {} - keyframedvalue<vector3>", i);
            let t = input.parse_le_u32("vector type");

            match t {
                0 => {}
                2 => {
                    let _ = input.parse_le_u32("?");
                    let translation = input.read_vec3("Translation");

                    track.frames.push(Frame {
                        time,
                        rotation: Quat::IDENTITY,
                        translation,
                    });
                }
                x => todo!("{}", x),
            }

            let _ = input.parse_n_bytes(1, "?");

            time += 1.0 / 30.0; // probably wrong
        }

        let _ = input.parse_le_u32("?");
        let t = input.read_vec3("Translation");
        track.frames.push(Frame {
            time,
            rotation: Quat::IDENTITY,
            translation: t,
        });

        tracks.push(track);
    }
}

fn parse_keyframedvalue_transform(
    qty: usize,
    input: &mut NomSlice,
    mapping: &ChecksumMap,
    animation_duration: f32,
    tracks: &mut Vec<Track>,
) {
    debug!("Track type: keyframedvalue<transform>");
    for _ in 0..qty {
        let mut track = Track {
            bone_name_hash: 0,
            bone_name: None,
            frames: vec![],
        };

        debug!("New Track - keyframedvalue<transform>");
        let header_length = input.parse_le_u32("Header length");
        assert!(header_length == 24);

        let subheader_length = input.parse_le_u32("Subheader length");
        assert!(subheader_length == 20);

        let bone_name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
            mapping.get_mapping(k).unwrap_or("?".to_string())
        });
        track.bone_name_hash = bone_name_hash;
        track.bone_name = mapping.get_mapping(bone_name_hash);

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("flags?");

        let (_, _) = input.read_length_transform("min");
        let (_, _) = input.read_length_transform("max");

        let _ = input.parse_le_u32("frames length in bytes");
        let qty = input.parse_le_u32("qty");

        for i in 0..=qty {
            debug!("New Keyframe {} - keyframedvalue<transform>", i);
            let t = input.parse_le_u32("type");

            match t {
                0 => {}
                2 => {
                    let (rotation, translation) = input.read_length_transform("Frame Transform");

                    let mut frame = Frame {
                        time: animation_duration,
                        rotation,
                        translation,
                    };

                    if i == qty {
                        // track.frames.push(frame);
                        break;
                    } else {
                        let time = input.parse_le_f32("Frame Time");
                        frame.time = time;
                        track.frames.push(frame);
                    }
                }
                x => todo!("{}", x),
            }

            let _ = input.parse_n_bytes(1, "?");
        }

        tracks.push(track);
    }
}

fn parse_keyframedvalue_quaternion(
    qty: usize,
    input: &mut NomSlice,
    mapping: &ChecksumMap,
    tracks: &mut Vec<Track>,
) {
    debug!("Track type: keyframedvalue<quaternion>");
    for _ in 0..qty {
        log::debug!("New Track - keyframedvalue<quaternion>");

        let mut track = Track {
            bone_name_hash: 0,
            bone_name: None,
            frames: vec![],
        };

        let header_length = input.parse_le_u32("Header length");
        assert!(header_length == 24);
        let subheader_length = input.parse_le_u32("Subheader length");
        assert!(subheader_length == 20);

        let bone_name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
            mapping.get_mapping(k).unwrap_or("?".to_string())
        });
        track.bone_name_hash = bone_name_hash;
        track.bone_name = mapping.get_mapping(bone_name_hash);

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");
        let _ = input.read_quat("min");
        let _ = input.read_quat("max");

        let _ = input.parse_le_u32("?");

        let qty = input.parse_le_u32("Quantity of Keyframes");

        let _ = input.parse_le_u32("?");
        let _ = input.parse_n_bytes(1, "?");

        let mut time = 0.0;
        for i in 0..qty {
            debug!("New Keyframe {} - keyframedvalue<quaternion>", i);
            let t = input.parse_le_u32("type?");

            match t {
                2 => {
                    let q = input.read_quat("rotation");

                    track.frames.push(Frame {
                        time,
                        rotation: q,
                        translation: Vec3::ONE,
                    });

                    if i == (qty - 1) {
                        break;
                    }
                }
                x => todo!("{}", x),
            }

            let _ = input.parse_n_bytes(1, "?");

            time += 1.0 / 30.0; // probably wrong
        }

        tracks.push(track);
    }
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
