use std::{io::Read, collections::HashMap};

use bitvec::{slice::BitSlice, order::Lsb0};

use crate::parser::{self, NomSlice};

#[derive(Debug)]
pub struct AnmFile {
    bones: Vec<Bone>
}

impl AnmFile {
    pub fn new() -> Self {
        Self {
            bones: vec![]
        }
    }

    pub fn push_animated_bone(&mut self, bone: AnimatedBone) {
        self.bones.push(Bone::Animated(bone));
    }
}

#[derive(Debug)]
pub struct AnimatedBoneFrame {
    compressed: bool,
    rotation: [f64;4],
    translation: [f64;3]
}

impl AnimatedBoneFrame {
    pub fn translation_as_f32(&self) -> [f32;3] {
        [
           self.translation[0] as f32,
           self.translation[1] as f32,
           self.translation[2] as f32,
        ]
    }

    pub fn rotation_as_f32(&self) -> [f32;4] {
        [
           self.rotation[0] as f32,
           self.rotation[1] as f32,
           self.rotation[2] as f32,
           self.rotation[3] as f32,
        ]
    }
}

#[derive(Debug)]
pub struct AnimatedBone {
    id: u64,
    frames: Vec<AnimatedBoneFrame>
}

impl AnimatedBone {
    pub fn new() -> Self {
        Self { 
            id: 0,
            frames: vec![]
        }
    }
}

#[derive(Debug)]
pub enum Bone {
    Animated(AnimatedBone)
}

impl Bone {
    pub fn as_animated(&self) -> &AnimatedBone {
        match &self {
            Bone::Animated(bone) => &bone,
        }
    }
}

pub struct Buffer {
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
    println!("\tdatapos: {} - {}th byte - {}th dword", datapos, datapos / 8, datapos / 32);
}

pub fn convert<P: AsRef<str>>(path: P) {
    let mut bar = progress::Bar::new();
    bar.set_job_title("Parsing...");

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

    let mut anm = AnmFile::new();

    let header = input.read_properties();
    println!("{:?}", header);

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u64("?");

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_f32("duration?");

    let _anim_length = input.parse_le_u32("animation length in bytes?");

    let qty_values = input.parse_le_u32("qtd values") as usize;
    let _length = input.parse_le_u32("length?");
    let qty_buffers = input.parse_le_u32("qty");

    let mut buffers = vec![];
    for _ in 0..qty_buffers {
        let hash = input.parse_le_u64("?");
        let hbytes = hash.to_le_bytes();
        let (hbytes, a) = parser::parse_le_u32(&hbytes[..]).unwrap();
        let (_, b) = parser::parse_le_u32(hbytes).unwrap();
        println!("{} {}", a, b);
        let a = input.parse_le_u32("?");
        let qty = input.parse_le_u16("?");
        let b = input.parse_le_u16("?");

        let i = Buffer { hash, qty, a, b };
        buffers.push(i);
    }

    let max_bounds = [0.00999999978, 0.0500000007, 0.100000001, 0.5,
        0.699999988, 0.800000012, 1.0, 1.5, 2.0, 3.0, 4.0, 5.5, 7.0, 8.5, 10.0];

    let mut bones = vec![];
    
    for buffer in buffers {
        println!("buffer: {:?}", buffer.hash);
        match buffer.hash {
            //CompressedTransformKeys
            0xFC6597EB1FE5458E => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();

                    let length = input.parse_n_bytes(1);
                    let length = if length[0] == 255 {
                        input.parse_le_u16("length")
                    } else {
                        length[0] as u16
                    };



                    let header = input.parse_n_bytes(length as usize);

                    if header.len() > 0 {

                        // 1111 1111 1111 1111 1111 1111 1111 1111/1111 1111 1111 1111 1111 1111 1111 1111
                        //                       |  sample count |                       |bps| mbi| bpb|
                        let (h, u32_0) = parser::parse_le_u32(header).unwrap();
                        let (_h, u32_1) = parser::parse_le_u32(h).unwrap();
                        let sample_count = (u32_0 & 0x3FFF) as usize;
                        let bits_per_sample = (((u32_1 >> 11) & 0x7) + 1) as usize;
                        let max_bounds_index = ((u32_1 >> 0x7) & 0xF) as usize;
                        let bits_per_bounds = ((u32_1 >> 0x3) & 0xF) as usize;

                        let mut bounds_sizes = vec![];
                        let bits = BitSlice::<Lsb0, _>::from_slice(&header[..]).unwrap();
                        for i in 0..7 {
                            let pos = 3*i + 0xE;
                            let v = skip_take_as_usize(bits, pos, 3);
                            bounds_sizes.push(v);
                        }

                        let max_bound = max_bounds[max_bounds_index];

                        let header32 = unsafe { std::slice::from_raw_parts(header.as_ptr() as *const u32, header.len() / 4) };
                        println!("{} {:?}", header32.len(), header32);
                        println!("Max Bounds: {:?} {} [{}]", bounds_sizes, max_bound, max_bounds_index);
                        println!("Sample Count: {}", sample_count);
                        println!("bits_per_sample: {}", bits_per_sample);
                        println!("max_bounds_index: {}", max_bounds_index);
                        println!("bits_per_bounds: {}", bits_per_bounds);
                        
                        let mut samples = 0;
                        let mut datapos = 50;

                        loop {
                            if samples >= sample_count {
                                break
                            }

                            println!("loop start: {}", samples);
                            println!("\tdatapos: {}", datapos);
                            
                            let mut bounds_bitsizes = vec![];
                            for i in 0..7 {
                                let idx = skip_take_as_usize(bits, datapos, bounds_sizes[i]);
                                datapos += bounds_sizes[i];
                                bounds_bitsizes.push(idx);
                            }
                            let bitsizessum = bounds_bitsizes.iter().sum::<usize>();
                            println!("\tBounds Bit sizes: {:?} {}", bounds_bitsizes, bitsizessum);
                            
                            print_datapos(datapos);
                            let qty_samples = skip_take_as_usize(bits, datapos, bits_per_sample);
                            datapos += bits_per_sample;

                            samples += qty_samples;

                            println!("\tQty Sample: {:?}", qty_samples);
                            println!("\tdatapos: {}", datapos);

                            let mut upper_value = 0.0;
                            let mut lower_value = 0.0;
                            if bitsizessum != 0 {
                                let b0 = skip_take_as_usize(bits, datapos, bits_per_bounds);
                                datapos += bits_per_bounds;
                                println!("\tSome Value: {}", b0);
                                if bits_per_bounds == 0 {
                                    upper_value = 2.0 * max_bound;
                                    lower_value = -max_bound;
                                } else {
                                    let max = ((1 << bits_per_bounds) - 1);
                                    let new_b0 = b0 & max;
                                    let new_b0 = (new_b0 as f64) / (max as f64) * max_bound;
                                    println!("\tSome Value: {} = {} / {} * {}", new_b0, b0, max, max_bound);
                                    upper_value = new_b0 * 2.0;
                                    lower_value = -new_b0;
                                    println!("\t{} {}", upper_value, lower_value);
                                }
                            }

                            for _ in 0..qty_samples {
                                println!("\tdatapos: {}", datapos);
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
                                        println!("\t{} * {} + {}", v, upper_value, lower_value);
                                        v * upper_value + lower_value
                                    };

                                    fsamples.push(v);
                                }
                                println!("\tSamples Values: {:?}", samples);
                                println!("\tSamples Values: {:?}", fsamples);

                                let frame = AnimatedBoneFrame {
                                    compressed: true,
                                    rotation: [fsamples[0], fsamples[1], fsamples[2], fsamples[3]],
                                    translation: [fsamples[4], fsamples[5], fsamples[6]],
                                };

                                bone.frames.push(frame);
                            }

                            println!("\tdatapos: {}", datapos);

                            println!("\tSamples: {}", samples);

                            if samples < sample_count {
                                let b = skip_take_as_usize(bits, datapos, 1);
                                datapos += 1;
                                println!("\t?: {:?}", b);
                            } else {
                                break
                            }

                            println!("\tdatapos: {}", datapos);
                        }

                        let all_padding = bits.iter().skip(datapos).all(|x| x == false); 

                        assert!(samples == sample_count);
                        if !all_padding {
                            assert!(datapos == header.len() * 8, "{} {} remaining {:?}", 
                                datapos, 
                                header.len()*8,
                                bits.iter().skip(datapos).map(|x| x == true).collect::<Vec<_>>()                         
                            );
                        }
                    }

                    let data = input.parse_length1_buffer("?");

                    println!("Second Buffer: {:?}", data);
                    {
                        let data = unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u32, data.len() / 4)};
                        println!("Second Buffer: {:?}", data);
                    }

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

                    println!("a: {}, b: {}, b2: {}, tblA: {}, c: {}, d: {}, e: {}", a, b, b2, tblA, c, d, e);

                    bones.push(bone);
                }
            }
            
            0x9A946F2A83FC7658 => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();
                    bone.frames.push(AnimatedBoneFrame {
                        compressed: false,
                        rotation: [0.0, 0.0, 0.0, 1.0],
                        translation: [0.0, 0.0, 0.0]
                    });
                    bones.push(bone);
                }
            }

            //singlevalue<vector3>
            0x6B77C806C0E23EA1 => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();
                    let t = input.read_vec3("vec3");
                    bone.frames.push(AnimatedBoneFrame {
                        compressed: false,
                        rotation: [0.0, 0.0, 0.0, 1.0],
                        translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                    });
                    bones.push(bone);
                }
            }

            // SingleValue<Quaternion>
            0xCECACE3A835CB7EE => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();
                    let q = input.read_quat("quat");
                    bone.frames.push(AnimatedBoneFrame {
                        compressed: false,
                        rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                        translation: [0.0, 0.0, 0.0],
                    });
                    bones.push(bone);
                }
            }
           
            // SingleValue<Transform>
            0xC1E84D6FF72CE80 => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();
                    let q = input.read_quat("quat");
                    let t = input.read_vec3("vec3");
                    bone.frames.push(AnimatedBoneFrame {
                        compressed: false,
                        rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                        translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                    });
                    bones.push(bone);
                }
            }
            
            // keyframedvalue<quaternion>
            0x1019453EB19C1ABD => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();

                    log::debug!("- new bone (quat)");
                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");
                    let boneid = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");
                    let _ = input.read_quat("?");
                    let _ = input.read_quat("?");

                    let _ = input.parse_le_u32("?");
                    
                    let qty = input.parse_le_u32("qty");

                    let t = input.parse_le_u32("?");
                    let _ = input.parse_n_bytes(1);

                    for i in 0..qty {
                        log::debug!("-- new quat");
                        let t = input.parse_le_u32("type");
    
                        match t {
                            2 => {
                                let q = input.read_quat("");

                                bone.frames.push(AnimatedBoneFrame{
                                    compressed: false,
                                    rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                                    translation: [0.0, 0.0, 0.0],
                                });

                                if i == (qty - 1) {
                                    break
                                }
                            }
                            x => todo!("{}", x)
                        }
    
                        let _ = input.parse_n_bytes(1);
                    }

                    bone.id = boneid;
                    bones.push(bone);
                }
            }
            
            // keyframedvalue<transform>
            0x5D3E9FC6FA9369BF => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();

                    log::debug!("- new bone transform");
                    let header_length = input.parse_le_u32("header length");
                    assert!(header_length == 24);
                    let subheader_length = input.parse_le_u32("subheader length");
                    assert!(subheader_length == 20);

                    let boneid = input.parse_le_u64("boneid");
                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");

                    let (minq, mint) = input.read_length_transform("min");
                    let (maxq, maxt) = input.read_length_transform("max");

                    let _ = input.parse_le_u32("frames length in bytes");
                    let qty = input.parse_le_u32("qty");

                    for i in 0..=qty {
                        log::debug!("- frame {}", i);
                        let t = input.parse_le_u32("type");

                        match t {
                            0 => {
                                let _ = input.parse_n_bytes(1);
                            }
                            2 => {
                                let (q, t) = input.read_length_transform("max");

                                bone.frames.push(AnimatedBoneFrame{
                                    compressed: false,
                                    rotation: [q[0] as f64, q[1] as f64, q[2] as f64, q[3] as f64],
                                    translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                                });

                                if i == qty {
                                    break
                                }

                                let _ = input.parse_le_f32("time");
                                let _ = input.parse_n_bytes(1);
                            }
                            x => todo!("{}", x)
                        }
                    }

                    bone.id = boneid;
                    bones.push(bone);
                }
            }
            
            //keyframedvalue<vector3>
            0xF6F394AF6E4003AD => {
                for _ in 0..buffer.qty {
                    let mut bone = AnimatedBone::new();
                    log::debug!("- new bone vec3");

                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                    let _ = input.parse_le_u32("?");

                    let _ = input.read_vec3("?");
                    let _ = input.read_vec3("?");

                    let _ = input.parse_le_u32("?");
                    let qty = input.parse_le_u32("?");

                    for i in 0..qty {
                        log::debug!("-- new vec3");
                        let t = input.parse_le_u32("?");

                        match t {
                            0 => {

                            }
                            2 => {
                                let _ = input.parse_le_u32("?");
                                let t = input.read_vec3("?");

                                bone.frames.push(AnimatedBoneFrame{
                                    compressed: false,
                                    rotation: [0.0, 0.0, 0.0, 0.0],
                                    translation: [t[0] as f64, t[1] as f64, t[2] as f64],
                                });
                            }
                            x => todo!("{}", x)
                        }

                        let _ = input.parse_n_bytes(1);
                    }

                    let _ = input.parse_le_u32("?");
                    let _ = input.read_vec3("?");

                    bones.push(bone);
                }
            }
            
            _ => todo!("{:X?}", buffer.hash)
        }
    }
    
    for i in 0..qty_values {
        let _ = input.parse_le_u32(format!("value: {}", i).as_str());
    }
    
    let _ = input.parse_le_u16("?");

    assert!(qty_values == bones.len(), "{} == {}", qty_values, bones.len());

    let mut byid = HashMap::new();
    for i in 0..qty_values {
        let k = input.parse_le_u64("Bone ID");
        let _ = input.parse_le_u32("");
        if bones[i].id == 0 {
            bones[i].id = k;
        }
    }

    for (i, bone) in bones.iter().enumerate() {
        byid.insert(bone.id, i);
    }

    let skl = crate::skl::parse_skl("C:\\temp\\mi101\\sk20_guybrush.skl");
    let mut gltf = crate::skl::gltf::to_gltf(&skl);

    let d3dmesh = crate::d3dmesh::parse_d3dmesh("C:\\temp\\mi101\\sk20_guybrush.d3dmesh").unwrap();
    crate::d3dmesh::outputs::gltf::add_mesh_to_gltf(&d3dmesh, &mut gltf);

    println!("{:#?}", skl);
    println!("{:#?}", skl.bones.len());
    println!("{:#?}", anm.bones.len());
    println!("{:#?}", anm);

    //time buffer
    

    let mut time_buffer = vec![];
    for sklbone in skl.bones.iter() {
        let bid = byid[&sklbone.start];
        let bone = &bones[bid];

        // let qty_frames = bone.frames.len();
        // let step = if qty_frames == 1 { 0.0 } else { 1.0 / (bone.frames.len() - 1) as f32 };
        let step = 1.0 / 30.0;

        let b: Vec<f32> = bone.frames.iter()
            .scan(-step, |t,_| { *t += step; Some(*t) })
            .collect();
        time_buffer.extend(b);
    }

    //rot buffer
    let mut rot_buffer = vec![];
    let mut t_buffer = vec![];
    for sklbone in skl.bones.iter() {
        let bid = byid[&sklbone.start];
        let bone = &bones[bid];
        //glam::Quat::from_array(sklbone.rotation)
        let b: Vec<f32> = bone.frames.iter()
            .scan(glam::Quat::default(), |acc, frame| {
                if frame.compressed {
                    *acc = *acc + glam::Quat::from_array(frame.rotation_as_f32());
                    let q = acc.normalize();
                    Some([q.x, q.y, q.z, q.w])
                } else {
                    Some(frame.rotation_as_f32())
                }
            })
            .flat_map(|x| x)
            .collect();        
        rot_buffer.extend(b);

        let b: Vec<f32> = bone.frames.iter()
            .scan(glam::Vec3::from_slice(&sklbone.translation[..]), |acc, frame| {
                let t = glam::Vec3::from_slice(&frame.translation_as_f32()[..]);
                *acc = *acc + t;
                Some(*acc.as_ref())
            })
            .flat_map(|x| x)
            .collect();
        t_buffer.extend(b);
    }

    let mut time_buffer_view = crate::gltf::push_buffer(&mut gltf, time_buffer.as_slice());
    time_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let time_buffer_idx = crate::gltf::push_buffer_view(&mut gltf, time_buffer_view);

    dbg!(&rot_buffer);
    let mut rot_buffer_view = crate::gltf::push_buffer(&mut gltf, rot_buffer.as_slice());
    rot_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let rot_buffer_idx = crate::gltf::push_buffer_view(&mut gltf, rot_buffer_view);

    let mut t_buffer_view = crate::gltf::push_buffer(&mut gltf, t_buffer.as_slice());
    t_buffer_view["target"] = json::JsonValue::Number(34963i32.into());
    let t_buffer_idx = crate::gltf::push_buffer_view(&mut gltf, t_buffer_view);

    let mut anim01 = json::object!{};

    let mut offset = 0;
    for (skli, sklbone) in skl.bones.iter().enumerate() {
        let bid = byid[&sklbone.start];
        let bone = &bones[bid];

        let time_acessor_idx = crate::gltf::push_accessor(&mut gltf, 
            json::object! {
                bufferView: time_buffer_idx,
                componentType: 5126,
                count: bone.frames.len(),
                type: "SCALAR",
                byteOffset: offset * 4,
            }
        );
        let rot_acessor_idx = crate::gltf::push_accessor(&mut gltf, json::object! {
            bufferView : rot_buffer_idx,
            componentType : 5126,
            count : bone.frames.len(),
            type : "VEC4",
            byteOffset: offset * 4 * 4,
        });
        let t_acessor_idx = crate::gltf::push_accessor(&mut gltf, json::object! {
            bufferView : t_buffer_idx,
            componentType : 5126,
            count : bone.frames.len(),
            type : "VEC3",
            byteOffset: offset * 3 * 4,
        });

        let sampler_idx = crate::gltf::push_sampler(&mut anim01, json::object! {
            input : time_acessor_idx,
            interpolation : "LINEAR",
            output : rot_acessor_idx
        });
        crate::gltf::push_channel(&mut anim01, json::object!{
            sampler: sampler_idx,
            target: json::object!{
                node: skli + 1,
                path: "rotation"
            }
        });

        let sampler_idx = crate::gltf::push_sampler(&mut anim01, json::object! {
            input : time_acessor_idx,
            interpolation : "LINEAR",
            output : t_acessor_idx
        });
        //TODO seems to be out of scale
        //maybe only makes sense when apllying the model
        // crate::gltf::push_channel(&mut anim01, json::object!{
        //     sampler: sampler_idx,
        //     target: json::object!{
        //         node: skli + 1,
        //         path: "translation"
        //     }
        // });

        offset += bone.frames.len();
    }

    gltf["animations"] = json::array![
        anim01
    ];

    crate::skl::gltf::save(".\\viewer\\models\\result.gltf", &gltf);

    println!("{:#?}", anm);
}
