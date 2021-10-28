use crate::d3dfile::NomSlice;

pub enum Attribute {
    None,
    BoundingBox(f32, f32, f32, f32, f32, f32),
    ATT0(String),
    ATT1(String),
    ATT2(String),
    ATT3(String),
    ATT4(String),
    ATT5(String),
    ATT6(String),
    ATT7(String),
    ATT8,
    ATT9(String),
    ATT10(String),
    ATT11(String),
    ATT12,
    ATT13(String),
    ATT14(String),
    ATT15(String),
    ATT16(String),
    ATT17(String),
    ATT18(String),
    ATT19(String),
    ATT20,
    ATT21(String),
    ATT22(String),
    ATT23(String),
    ATT24(String),
    ATT25(String),
    ATT26(String),
    ATT27(String),
    ATT28(String),
    ATT29(String),
    ATT30(String),
    ATT31(String),
    ATT32(String),
    ATT33(String),
    ATT34(String),
    ATT35(String),
    ATT36(String),
    ATT37(String),
    ATT38(String),
    ATT39(String),
    ATT40(String),
    ATT41(String),
    ATT42(String),
    ATT43(String),
    ATT44(String),
    ATT45(String),
    ATT46(String),
    ATT47(String),
    ATT48(String),
    ATT49(String),
    ATT50(String),
    ATT51(String),
    ATT52(String),
    ATT53(String),
    ATT54(String),
    ATT55(String),
    ATT56(String),
    ATT57(String),
    ATT58(String),
    ATT59(String),
    ATT60(String),
}

pub fn read_att(input: &mut NomSlice) -> Attribute {
    let att = input.parse_le_u32("attribute");
    match att {
        0 => {
            let minx = input.parse_le_f32("bbox.minx");
            let miny = input.parse_le_f32("bbox.miny");
            let minz = input.parse_le_f32("bbox.minz");
            let maxx = input.parse_le_f32("bbox.maxx");
            let maxy = input.parse_le_f32("bbox.maxy");
            let maxz = input.parse_le_f32("bbox.maxz");
            Attribute::BoundingBox(minx, miny, minz, maxx, maxy, maxz)
        }
        8 => {
            let _ = input.parse_le_u32("?");
            Attribute::ATT8
        }
        12 => {
            let _ = input.parse_le_u32("?");
            let _ = input.parse_le_u16("?");
            let _ = input.parse_le_u16("?");
            Attribute::ATT12
        }
        18 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT18(name.to_string())
        }
        20 => {
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            let _ = input.parse_le_f32("?");
            Attribute::ATT20
        }
        23 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT23(name.to_string())
        }
        24 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT24(name.to_string())
        }
        25 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT25(name.to_string())
        }
        26 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT26(name.to_string())
        }
        27 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT27(name.to_string())
        }
        28 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT28(name.to_string())
        }
        29 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT29(name.to_string())
        }
        30 => {
            let name = input.parse_length_string("some texture");
            Attribute::ATT30(name.to_string())
        }
        31 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT31(name.to_string())
        }
        32 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT32(name.to_string())
        }
        33 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT33(name.to_string())
        }
        34 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT34(name.to_string())
        }
        35 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT35(name.to_string())
        }
        36 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT36(name.to_string())
        }
        37 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT37(name.to_string())
        }
        38 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT38(name.to_string())
        }
        39 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT39(name.to_string())
        }
        40 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT40(name.to_string())
        }
        41 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT41(name.to_string())
        }
        42 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT42(name.to_string())
        }
        43 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT43(name.to_string())
        }
        44 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT44(name.to_string())
        }
        45 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT45(name.to_string())
        }
        46 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT46(name.to_string())
        }
        47 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT47(name.to_string())
        }
        48 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT48(name.to_string())
        }
        49 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT49(name.to_string())
        }
        50 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT50(name.to_string())
        }
        51 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT51(name.to_string())
        }
        52 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT52(name.to_string())
        }
        53 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT53(name.to_string())
        }
        54 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT54(name.to_string())
        }
        55 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT54(name.to_string())
        }
        57 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT57(name.to_string())
        }
        58 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT58(name.to_string())
        }
        59 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT59(name.to_string())
        }
        60 => {
            let name = input.parse_length_string("?? map");
            Attribute::ATT60(name.to_string())
        }
        64 => {
            let name = input.parse_length_string("?? map");
            Attribute::None
        }
        x @ _ => todo!("{:?}", x),
    }
}

pub fn read_att2(input: &mut NomSlice) -> Attribute {
    let att = input.parse_le_u32("attribute");
    match att {
        8 => {
            let _ = input.parse_le_u32("?");
            Attribute::ATT8
        }
        36 => {
            //example: obj_ballcannon
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        48 => {
            //example: obj_ballcannon
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        60 => {
            //example: obj_dollscorpion
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        72 => {
            //example: obj_flagflotsam
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        84 => {
            //example: obj_idolmerfolkd
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        96 => {
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        108 => {
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        120 => {
            //sk39_monkeyelectrocute.obj
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        144 => {
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        180 => {
            //obj_clothesline
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        340 => {
            //?
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        500 => {
            //sk37_seagull.obj
            let _ = input.parse_le_u32("?");
            let qty = input.parse_le_u32("qty?");
            for _ in 0..qty {
                let _ = input.parse_le_u64("?");
                let _ = input.parse_le_u32("?");
            }
            Attribute::None
        }
        1036 => {
            //sk39_monkeyarms.d3dmesh
            let qty = input.parse_le_u32("1696:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1696:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        1548 => {
            //sk20_pirateacrimpdigit.d3dmesh
            let qty = input.parse_le_u32("1696:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1696:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        1660 => {
            //sk20_pirateamcgeepox.d3dmesh
            let qty = input.parse_le_u32("1696:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1696:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        1696 => {
            //sk13_scorpionmonkey.d3dmesh
            let qty = input.parse_le_u32("1696:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1696:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        1732 => {
            //sk20_pirateamcgeepox.d3dmesh
            let qty = input.parse_le_u32("1768:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1768:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        1768 => {
            //sk14_lechuckhuman.d3dmesh
            let qty = input.parse_le_u32("1768:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1768:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        1928 => {
            //sk03_elaine.d3dmesh
            let qty = input.parse_le_u32("1928:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1928:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2052 => {
            //sk20_desinge.d3dmesh
            let qty = input.parse_le_u32("1928:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1928:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2088 => {
            //sk39_monkey.d3dmesh
            let qty = input.parse_le_u32("1928:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("1928:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2208 => {
            //sk20_guybrush.d3dmesh
            let qty = input.parse_le_u32("2208:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("2208:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2248 => {
            //sk04_voodoolady.d3dmesh
            let qty = input.parse_le_u32("2248:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("2248:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2368 => {
            //sk15_piratebwinslow.d3dmesh
            let qty = input.parse_le_u32("2248:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("2248:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2272 => {
            //sk15_piratebdoro.d3dmesh
            let qty = input.parse_le_u32("2248:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("2248:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2688 => {
            //sk15_piratebnipperkin.d3dmesh
            let qty = input.parse_le_u32("2248:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("2248:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2712 => {
            //sk14_lechuckblue.d3dmesh
            let qty = input.parse_le_u32("2712:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("2712:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        2196 => {
            //sk14_lechuckdemon.d3dmesh
            let qty = input.parse_le_u32("2196:1");
            for _ in 0..qty {
                let qty = input.parse_le_u32("2196:qty");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("?");
                    let _ = input.parse_le_u32("?");
                }
            }
            Attribute::None
        }
        x @ _ => todo!("{:?}", x),
    }
}
