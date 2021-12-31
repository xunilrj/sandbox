use std::io::Read;

use bitvec::{slice::BitSlice, order::Lsb0};

use crate::parser::{self, NomSlice};

pub struct AnmFile {}

impl AnmFile {
    pub fn new() -> Self {
        Self {}
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
    assert!(v.len() == take);
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

    let _anm = AnmFile::new();

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
    
    for buffer in buffers {
        println!("buffer: {:?}", buffer.hash);
        match buffer.hash {
            //CompressedTransformKeys
            0xFC6597EB1FE5458E => {
                for _i in 0..buffer.qty {
                    let length = input.parse_n_bytes(1);
                    let length = if length[0] == 255 {
                        input.parse_le_u16("length")
                    } else {
                        length[0] as u16
                    };

                    let header = input.parse_n_bytes(length as usize);
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
                        
                        // if bitsizessum == 0 {
                        //     for i in datapos..(datapos + 100) {
                        //         let mut pos = i;
                        //         let mut bounds_bitsizes = vec![];
                        //         for i in 0..7 {
                        //             let idx = skip_take_as_usize(bits, pos, bounds_sizes[i]);
                        //             pos += bounds_sizes[i];
                        //             bounds_bitsizes.push(idx);
                        //         }
                        //         println!("{} - {:?}", i, bounds_bitsizes);
                        //     }
                        // }

                        print_datapos(datapos);
                        let qty_samples = skip_take_as_usize(bits, datapos, bits_per_sample);
                        datapos += bits_per_sample;

                        // if qty_samples == 0 {
                        //     println!("\tOriginal Qty Sample: {:?}", qty_samples);
                        //     qty_samples = sample_count - samples;
                        // }

                        samples += qty_samples;

                        println!("\tQty Sample: {:?}", qty_samples);
                        println!("\tdatapos: {}", datapos);

                        // if qty_samples == 0 {
                        //     continue;
                        // }

                        if bitsizessum != 0 {
                            let b0 = skip_take_as_usize(bits, datapos, bits_per_bounds);
                            datapos += bits_per_bounds;
                            println!("\tSome Value: {}", b0);
                            let max = ((2usize).pow(bits_per_bounds as u32) - 1) as f64;
                            let b0 = (b0 as f64) / max;
                            println!("\tSome Value: {}", b0);
                        }
                        

                        for _ in 0..qty_samples {
                            println!("\tdatapos: {}", datapos);
                            let mut samples = vec![];
                            for size in &bounds_bitsizes {
                                let v = skip_take_as_usize(bits, datapos, *size);
                                datapos += size;
                                // let max = ((2usize).pow(*size as u32) - 1) as f64;
                                // let v = (v as f64) / max * max_bound;
                                samples.push(v);
                            }
                            println!("\tSamples Values: {:?}", samples);
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

                    let all_padding = bits.iter().skip(datapos).any(|x| x == true) == false; 

                    assert!(samples == sample_count);
                    if !all_padding {
                        assert!(datapos == header.len() * 8, "{} {} remaining {:?}", 
                            datapos, 
                            header.len()*8,
                            bits.iter().skip(datapos).map(|x| x == true).collect::<Vec<_>>()                         
                        );
                    }

                    let data = input.parse_length1_buffer("?");
                    let _a = data[0] & 7;

                    // println!("data: {} {} {} {}", a, 14 + a, a >> 14, a >> 3 & 7 + 1);
                }
            }
            0x6B77C806C0E23EA1 => {
                for _ in 0..buffer.qty {
                    let _v = input.read_vec3("vec3");
                }
            }

            // SingleValue<Quaternion>
            0xCECACE3A835CB7EE => {
                for _ in 0..buffer.qty {
                    let _q = input.read_quat("quat");
                }
            }
            // SingleValue<Transform>
            0xC1E84D6FF72CE80 => {
                for _ in 0..buffer.qty {
                    let _ = input.read_quat("quat");
                    let _ = input.read_vec3("vec3");
                }
            }
            _ => todo!("{:?}", buffer.hash)
        }
    }
    
    for i in 0..qty_values {
        let _ = input.parse_le_f32(format!("value: {}", i).as_str());
    }
    
    let skip_symbols = input.parse_le_u16("?");
    if skip_symbols == 0 {
        let _ = input.read_n_properties(qty_values);
    }
}
