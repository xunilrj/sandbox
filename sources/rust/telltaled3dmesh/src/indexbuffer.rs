use std::iter::FromIterator;

use iced_x86::Register;

use crate::run::{Context, Value};

pub fn get_index_buffer(
    ib: Vec<u8>,
    esp14: &mut u32, //first index
    esp18: &mut u32,
    esp20: &mut u32,
    esp40: &mut u32,
    esi1c: &mut u32,
    esi20: &mut u32,
) -> Vec<u8> {
    let mut ctx = Context::new();
    ctx.debug = false;
    ctx.mount_mem(0x19E000, vec![0u8; 4 * 1024]);
    ctx.mount_mem(0x0070FC81, crate::run::read_code());
    ctx.mount_mem(0xEF14F28, vec![0u8; 100]);
    ctx.mount_mem(0xF8200A0, vec![0u8; 1024 * 1024]);
    // ctx.mount_mem_file(0x3BD3020, "island_gelato_indexbuffer.txt");
    ctx.mount_mem(0x3BD3020, ib);
    ctx.set_register(Register::EIP, 0x0070FC81);
    ctx.set_register(Register::ESP, 0x19E074);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x10), 0x0);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x14), *esp14);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x18), *esp18);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x1C), 0x0);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x20), *esp20);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x24), 0xEF14F28);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x38), 0x3BD3020);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x40), *esp40);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x1C), *esi1c);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x20), *esi20);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x24), 0xF8200A0);
    ctx.set_value(Value::Memory(0xF8200A0 + 0x00), *esp14);
    ctx.break_if_eip(0x0070FEA1);

    crate::run::run(&mut ctx, usize::MAX - 1);

    let output = ctx.borrow_mem(0xF8200A0);
    return Vec::from_iter(output.iter().map(|x| *x).take(*esi1c as usize * 2));
}
