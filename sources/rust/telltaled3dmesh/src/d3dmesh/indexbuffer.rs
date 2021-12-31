
use iced_x86::Register;
use log::debug;

use crate::emulator::{Context, Value};

pub fn get_index_buffer(
    ib: Vec<u8>,
    esp14: &mut u32, //first index
    esp18: &mut u32,
    esp20: &mut u32,
    esp40: &mut u32,
    esi1c: &mut u32, //qty indices
    esi20: &mut u32,
) -> Vec<u8> {
    debug!("esp14={}", esp14);
    debug!("esp18={}", esp18);
    debug!("esp20={}", esp20);
    debug!("esp40={}", esp40);
    debug!("esi1c={}", esi1c);
    debug!("esi20={}", esi20);
    let output_buffer_size = *esi1c as usize * 2;
    let mut ctx = Context::new();
    ctx.debug = std::env::var("EMULATOR_DEBUG")
        .unwrap_or("false".to_string())
        .parse()
        .unwrap();
    ctx.mount_mem(0x19E000, vec![0u8; 4 * 1024]);
    ctx.mount_mem(0x0070FC81, crate::emulator::read_code());
    ctx.mount_mem(0xEF14F28, vec![0u8; 100]);
    ctx.mount_mem(0xF8200A0, vec![0u8; output_buffer_size]);
    ctx.mount_mem(0x3BD3020, ib);
    ctx.set_register(Register::EIP, 0x0070FC81);
    ctx.set_register(Register::ESP, 0x19E074);
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x10),
        Value::U32(0x0),
    );
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x14),
        Value::U32(*esp14),
    );
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x18),
        Value::U32(*esp18),
    );
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x1C),
        Value::U32(0x0),
    );
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x20),
        Value::U32(*esp20),
    );
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x24),
        Value::U32(0xEF14F28),
    );
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x38),
        Value::U32(0x3BD3020),
    );
    ctx.set_value(
        Value::MemoryRegisterValue(Register::ESP, 0x40),
        Value::U32(*esp40),
    );
    ctx.set_value(Value::Memory(0xEF14F28 + 0x1C), Value::U32(*esi1c));
    ctx.set_value(Value::Memory(0xEF14F28 + 0x20), Value::U32(*esi20));
    ctx.set_value(Value::Memory(0xEF14F28 + 0x24), Value::U32(0xF8200A0));
    ctx.set_value(Value::Memory(0xF8200A0 + 0x00), Value::U32(*esp14));
    ctx.break_if_eip(0x0070FEA1);

    let max_steps = std::env::var("EMULATOR_MAXSTEPS")
        .unwrap_or("18446744073709551615".to_string())
        .parse()
        .unwrap();
    debug!("max steps: {}", max_steps);
    crate::emulator::run(&mut ctx, max_steps);
    debug!("done.");

    let output = ctx.borrow_mem(0xF8200A0);
    let v = output.iter().map(|x| *x).take(output_buffer_size).collect();

    debug!("retrieving index buffer");
    v
}
