use crate::parser::NomSlice;

#[derive(Debug)]
pub struct BonePallete {
    pub bones: Vec<usize>,
}

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
    U32(u32),
    // ATT9(String),
    // ATT10(String),
    // ATT11(String),
    U32U16U16(u32, u16, u16),
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
    OcclusionTextureMap(String),
    TextureMap(String),
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
    BonePallete(Vec<BonePallete>),
}

pub fn read_att(input: &mut NomSlice) -> Attribute {
    let att = input.parse_le_u32("attribute");
    match att {
        8 => {
            let a = input.parse_le_u32("?");
            Attribute::U32(a)
        }
        12 => {
            let a = input.parse_le_u32("?");
            let b = input.parse_le_u16("?");
            let c = input.parse_le_u16("?");
            Attribute::U32U16U16(a, b, c)
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
        32 => {
            let name = input.parse_length_string("occlusion texture map");
            Attribute::OcclusionTextureMap(name.to_string())
        }
        23..=64 => {
            let name = input.parse_length_string("some texture map");
            Attribute::TextureMap(name.to_string())
        }
        x @ _ => todo!("{:?}", x),
    }
}

pub fn read_att2(input: &mut NomSlice) -> Attribute {
    let att = input.parse_le_u32("attribute");
    match att {
        8 => {
            let a = input.parse_le_u32("?");
            Attribute::U32(a)
        }
        2208 => {
            let mut palletes = vec![];
            log::debug!("Bones palletes");
            let qty = input.parse_le_u32("qty of palletes");
            for _ in 0..qty {
                let mut pallete = BonePallete { bones: vec![] };
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
