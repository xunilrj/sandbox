use crate::parser::NomSlice;

use super::d3dfile::BonePallete;

pub enum Attribute {
    None,
    BoundingBox(f32, f32, f32, f32, f32, f32),
    // ATT0(String),
    // ATT1(String),
    // ATT2(String),
    // ATT3(String),
    // ATT4(String),
    // ATT5(String),
    // ATT6(String),
    // ATT7(String),
    ATT8,
    // ATT9(String),
    // ATT10(String),
    // ATT11(String),
    ATT12,
    // ATT13(String),
    // ATT14(String),
    // ATT15(String),
    // ATT16(String),
    // ATT17(String),
    ATT18(String),
    // ATT19(String),
    ATT20,
    // ATT21(String),
    // ATT22(String),
    ATT23(String),
    // ATT24(String),
    // ATT25(String),
    // ATT26(String),
    // ATT27(String),
    // ATT28(String),
    // ATT29(String),
    // ATT30(String),
    // ATT31(String),
    // ATT32(String),
    // ATT33(String),
    // ATT34(String),
    // ATT35(String),
    // ATT36(String),
    // ATT37(String),
    // ATT38(String),
    // ATT39(String),
    // ATT40(String),
    // ATT41(String),
    // ATT42(String),
    // ATT43(String),
    // ATT44(String),
    // ATT45(String),
    // ATT46(String),
    // ATT47(String),
    // ATT48(String),
    // ATT49(String),
    // ATT50(String),
    // ATT51(String),
    // ATT52(String),
    // ATT53(String),
    // ATT54(String),
    // ATT55(String),
    // ATT56(String),
    // ATT57(String),
    // ATT58(String),
    // // ATT59(String),
    // ATT60(String),
    BonePallete(Vec<BonePallete>)
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
        23..=64 => {
            let name = input.parse_length_string("some texture map");
            Attribute::ATT23(name.to_string())
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
        2208 => {
            let mut palletes = vec![];           
            log::debug!("Bones palletes");
            let qty = input.parse_le_u32("qty of palletes");
            for _ in 0..qty {
                let mut pallete = BonePallete {
                    bones: vec![],
                };
                let qty = input.parse_le_u32("qty of bones");
                for _ in 0..qty {
                    let boneid = input.parse_le_u64("bone id") as usize;
                    let _ = input.parse_le_u32("zero");

                    pallete.bones.push(boneid);
                }
                palletes.push(pallete);
            }
            Attribute::BonePallete(palletes)
        }
        36.. => {
            //example: obj_ballcannon
            let qty = input.parse_le_u32("qty of objects");
            for _ in 0..qty {
                let qty = input.parse_le_u32("qty of properties");
                for _ in 0..qty {
                    let _ = input.parse_le_u64("prop hash");
                    let _ = input.parse_le_u32("prop value");
                }
            }
            Attribute::None
        }
        x @ _ => todo!("{:?}", x),
    }
}

