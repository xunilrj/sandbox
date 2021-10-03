use iced_x86::{Decoder, Formatter, Instruction, NasmFormatter, Register};
use rust_lapper::{Interval, Lapper};
use std::collections::HashMap;

fn low_u8(v: u32) -> u8 {
    (v & 0xFF) as u8
}

fn low_u16(v: u32) -> u16 {
    (v & 0xFFFF) as u16
}

type Iv = Interval<usize, u32>;
pub struct Context {
    pub debug: bool,
    of: bool,
    sf: bool,
    cf: bool,
    zf: bool,
    eip: u32,
    eax: u32,
    ebx: u32,
    ecx: u32,
    edx: u32,
    registers: HashMap<String, u32>,
    intervals: Vec<Iv>,
    mem: Lapper<usize, u32>,
    mems: Vec<Vec<u8>>,
    log: String,
    eip_breaker: u32,
}

impl Context {
    pub fn new() -> Self {
        Self {
            debug: false,
            of: false,
            sf: false,
            cf: false,
            zf: false,
            eip: 0,
            eax: 0,
            ebx: 0,
            ecx: 0,
            edx: 0,
            registers: HashMap::new(),
            intervals: vec![],
            mem: Lapper::new(vec![]),
            mems: vec![],
            log: String::new(),
            eip_breaker: 0xFFFFFFFF,
        }
    }

    pub fn break_if_eip(&mut self, breaker: u32) {
        self.eip_breaker = breaker;
    }

    pub fn mount_mem(&mut self, addr: usize, v: Vec<u8>) {
        self.intervals.push(Interval {
            start: addr,
            stop: addr + v.len(),
            val: self.mems.len() as u32,
        });
        self.mems.push(v);
        self.mem = Lapper::new(self.intervals.clone());
    }

    pub fn mount_mem_file(&mut self, addr: usize, path: &str) {
        let v = std::fs::read(path).unwrap();
        self.mount_mem(addr, v)
    }

    pub fn get_register(&mut self, register: iced_x86::Register) -> u32 {
        match register {
            Register::EIP => return self.eip,
            Register::EAX => return self.eax,
            Register::EBX => return self.ebx,
            Register::ECX => return self.ecx,
            Register::EDX => return self.edx,
            _ => {}
        }

        let name = format!("{:?}", register);
        if let Some(r) = self.registers.get(&name) {
            if self.debug {
                self.log
                    .push_str(format!(" read {}=0x{:X?}", name, r).as_str());
            }
            *r
        } else {
            match name.as_str() {
                "AX" => {
                    let v = self.get_register(Register::EAX);
                    low_u16(v).into()
                }
                "DX" => {
                    let v = self.get_register(Register::EDX);
                    low_u16(v).into()
                }
                "CL" => {
                    let v = self.get_register(Register::ECX);
                    low_u8(v).into()
                }
                _ => 0,
            }
        }
    }

    pub fn set_register(&mut self, register: iced_x86::Register, v: u32) {
        match register {
            Register::EIP => {
                self.eip = v;
                if self.debug {
                    self.log.push_str(format!(" write EIP=0x{:X?}", v).as_str());
                }
                return;
            }
            Register::EAX => {
                self.eax = v;
                if self.debug {
                    self.log.push_str(format!(" write EAX=0x{:X?}", v).as_str());
                }
                return;
            }
            Register::EBX => {
                self.ebx = v;
                if self.debug {
                    self.log.push_str(format!(" write EBX=0x{:X?}", v).as_str());
                }
                return;
            }
            Register::ECX => {
                self.ecx = v;
                if self.debug {
                    self.log.push_str(format!(" write ECX=0x{:X?}", v).as_str());
                }
                return;
            }
            Register::EDX => {
                self.edx = v;
                if self.debug {
                    self.log.push_str(format!(" write EDX=0x{:X?}", v).as_str());
                }
                return;
            }
            _ => {
                let name = format!("{:?}", register);
                let r = self.registers.entry(name.clone()).or_default();
                *r = v;

                if self.debug {
                    self.log
                        .push_str(format!(" write {}=0x{:X?}", name, v).as_str());
                }
            }
        }
    }

    pub fn solve_u32(&mut self, addr: Value) -> u32 {
        match addr {
            Value::MemoryRegisterValue(base, delta) => {
                let addr = self.get_register(base);
                self.read_at((addr + delta) as usize)
            }
            Value::Memory(addr) => self.read_at((addr) as usize),
            Value::Register(r) => self.get_register(r),
            Value::U8(v) => v as u32,
            Value::U32(v) => v,
        }
    }

    fn get_mem_vec_mut(&mut self, addr: usize) -> (usize, &mut Vec<u8>) {
        let mem: Vec<_> = self.mem.find(addr as usize, addr as usize + 1).collect();

        if mem.len() == 0 {
            panic!("Memory not found: {} 0x{:X?}", addr, addr);
        }

        let area = mem[0];
        let start = area.start;
        let mem = &mut self.mems[area.val as usize];

        (start, mem)
    }

    fn get_mem_vec(&self, addr: usize) -> (usize, &Vec<u8>) {
        let mem: Vec<_> = self.mem.find(addr as usize, addr as usize + 1).collect();

        if mem.len() == 0 {
            panic!("Memory not found: {} 0x{:X?}", addr, addr);
        }

        let area = mem[0];
        let start = area.start;
        let mem = &self.mems[area.val as usize];

        (start, mem)
    }

    pub fn borrow_mem(&self, addr: usize) -> &Vec<u8> {
        let mem: Vec<_> = self.mem.find(addr as usize, addr as usize + 1).collect();

        if mem.len() == 0 {
            panic!("Memory not found: {} 0x{:X?}", addr, addr);
        }

        let area = mem[0];
        &self.mems[area.val as usize]
    }

    pub fn set_value(&mut self, dst: Value, v: u32) {
        match dst {
            Value::Register(register) => self.set_register(register, v),
            Value::MemoryRegisterValue(base, delta) => {
                let base = self.get_register(base);
                let addr = base + delta;
                let (start, mem) = self.get_mem_vec_mut(addr as usize);
                let addr = addr as usize - start;
                let addr: &mut u32 = unsafe { std::mem::transmute(&mut mem[addr]) };
                *addr = v;

                if self.debug {
                    self.log
                        .push_str(format!(" write mem[0x{:X?}]=0x{:X?}", base + delta, v).as_str());
                }
            }
            Value::Memory(addr) => {
                let (start, mem) = self.get_mem_vec_mut(addr as usize);
                let addr = addr as usize - start;
                let addr: &mut u32 = unsafe { std::mem::transmute(&mut mem[addr]) };
                *addr = v;

                if self.debug {
                    self.log
                        .push_str(format!(" write mem[0x{:X?}]=0x{:X?}", addr, v).as_str());
                }
            }
            Value::U8(_) => panic!("Impossible"),
            Value::U32(_) => panic!("Impossible"),
        }
    }

    pub fn set_value_op0(&mut self, i: &Instruction, v: u32) {
        let addr = Value::from_op0(&i);
        self.set_value(addr, v);
    }

    pub fn read_op0(&mut self, i: &Instruction) -> u32 {
        self.solve_u32(Value::from_op0(&i))
    }

    pub fn read_op1(&mut self, i: &Instruction) -> u32 {
        self.solve_u32(Value::from_op1(&i))
    }

    pub fn read_op0_1(&mut self, i: &Instruction) -> (u32, u32) {
        (
            self.solve_u32(Value::from_op0(&i)),
            self.solve_u32(Value::from_op1(&i)),
        )
    }

    pub fn register_incr(&mut self, register: iced_x86::Register, incr: isize) {
        let v = self.get_register(register) as isize + incr;
        self.set_register(register, v as u32)
    }

    pub fn read_eip(&mut self) -> (u32, &[u8]) {
        let eip = self.get_register(iced_x86::Register::EIP);
        let (start, mem) = self.get_mem_vec(eip as usize);
        let i = eip as usize - start;
        let mem = mem.as_slice();
        (eip, &mem[i..])
    }

    pub fn read_at<T: Copy + std::fmt::Debug>(&mut self, addr: usize) -> T {
        if self.debug {
            self.log
                .push_str(format!(" read mem[0x{:X?}]=", addr).as_str());
        }

        let (start, mem) = self.get_mem_vec(addr);
        let i = addr - start;
        let mem = mem.as_slice();

        let v = unsafe { *(&mem[i] as *const u8 as *const T) };
        if self.debug {
            self.log.push_str(format!("0x{:X?}", v).as_str());
        }

        v
    }
}

pub enum Value {
    Register(iced_x86::Register),
    Memory(u32),
    MemoryRegisterValue(iced_x86::Register, u32),
    U8(u8),
    U32(u32),
}

impl Value {
    fn from_memory(i: &Instruction) -> Value {
        let base = i.memory_base();
        let displacement = i.memory_displacement32();
        Value::MemoryRegisterValue(base, displacement)
    }
    pub fn from_op0(i: &Instruction) -> Value {
        match i.op0_kind() {
            iced_x86::OpKind::Memory => Self::from_memory(i),
            iced_x86::OpKind::Register => Value::Register(i.op0_register()),
            iced_x86::OpKind::Immediate8 => Value::U8(i.immediate8()),
            iced_x86::OpKind::Immediate8to32 => Value::U32(i.immediate8to32() as u32),
            iced_x86::OpKind::Immediate32 => Value::U32(i.immediate32() as u32),
            iced_x86::OpKind::NearBranch32 => Value::U32(i.near_branch32()),
            x @ _ => todo!("{:?}", x),
        }
    }

    pub fn from_op1(i: &Instruction) -> Value {
        match i.op1_kind() {
            iced_x86::OpKind::Memory => Self::from_memory(i),
            iced_x86::OpKind::Register => Value::Register(i.op1_register()),
            iced_x86::OpKind::Immediate8 => Value::U8(i.immediate8()),
            iced_x86::OpKind::Immediate8to32 => Value::U32(i.immediate8to32() as u32),
            iced_x86::OpKind::Immediate32 => Value::U32(i.immediate32() as u32),
            x @ _ => todo!("{:?}", x),
        }
    }
}

pub fn read_code() -> Vec<u8> {
    let r = include_bytes!("../ibf.txt").to_vec();
    let r = String::from_utf8(r).unwrap();
    let r: Vec<_> = r
        .split(" ")
        .map(|x| u8::from_str_radix(x, 16).unwrap())
        .collect();
    r
}

pub fn run(ctx: &mut Context, steps: usize) {
    let mut i = Instruction::default();

    let mut str = String::new();
    let mut f = NasmFormatter::new();

    let original_steps = steps;
    let mut steps = steps;
    loop {
        let breaker = ctx.eip_breaker.clone();
        let (eip, data) = ctx.read_eip();

        if eip >= breaker {
            break;
        }

        if steps <= 0 {
            break;
        }
        steps -= 1;

        let mut decoder = Decoder::new(32, data, 0);

        if !decoder.can_decode() {
            break;
        }

        decoder.decode_out(&mut i);
        str.clear();
        f.format(&i, &mut str);

        if ctx.debug {
            print!("0x{:X?} {:?}", eip, str);
        }
        ctx.log.clear();
        match i.mnemonic() {
            iced_x86::Mnemonic::Mov => {
                let v = ctx.read_op1(&i);
                ctx.set_value_op0(&i, v);
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Movzx => {
                let v = ctx.read_op1(&i);
                ctx.set_value_op0(&i, v);
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::And => {
                let (dst, src) = ctx.read_op0_1(&i);
                let v = dst & src;
                ctx.set_value_op0(&i, v);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = false;
                ctx.zf = v == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Or => {
                let (dst, src) = ctx.read_op0_1(&i);
                let v = dst | src;
                ctx.set_value_op0(&i, v);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = false;
                ctx.zf = v == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Add => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_add(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Sub => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_sub(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Imul => {
                let (dst, src) = ctx.read_op0_1(&i);
                let dst = dst as i64;
                let src = src as i64;
                let (wrapped_value, of) = dst.overflowing_mul(src);
                ctx.set_value_op0(&i, wrapped_value as u32);
                ctx.of = of;
                ctx.sf = wrapped_value < 0;
                ctx.cf = false;
                ctx.zf = wrapped_value == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Shl => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_shl(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Shr => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_shr(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Neg => {
                let v = 0isize - ctx.read_op0(&i) as isize;
                let v = v as u32;
                ctx.set_value_op0(&i, v);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = false;
                ctx.zf = v == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Cmp => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_sub(src);
                ctx.of = false;
                ctx.sf = cf;
                ctx.cf = cf;
                ctx.zf = wrapped_value == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Test => {
                let (dst, src) = ctx.read_op0_1(&i);
                let v = dst & src;
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = false;
                ctx.zf = v == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Jmp => {
                let addr = ((ctx.read_op0(&i) as isize) + (eip as isize)) as usize;
                ctx.set_register(iced_x86::Register::EIP, addr as u32);
            }
            iced_x86::Mnemonic::Je => {
                if ctx.zf {
                    let addr = ((ctx.read_op0(&i) as isize) + (eip as isize)) as usize;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jne => {
                if !ctx.zf {
                    let addr = ((ctx.read_op0(&i) as isize) + (eip as isize)) as usize;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jae => {
                if !ctx.cf || ctx.zf {
                    let addr = ((ctx.read_op0(&i) as isize) + (eip as isize)) as usize;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jle => {
                if ctx.sf != ctx.of || ctx.zf {
                    let addr = ((ctx.read_op0(&i) as isize) + (eip as isize)) as usize;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jl => {
                if ctx.sf != ctx.of {
                    let addr = ((ctx.read_op0(&i) as isize) + (eip as isize)) as usize;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            x @ _ => todo!("{}, todo: {:?}", original_steps - steps as usize, x),
        }

        if ctx.debug {
            println!(" {}", ctx.log);
        }
    }
}

#[test]
pub fn test_emulator() {
    let t1 = std::time::Instant::now();

    let mut ctx = Context::new();
    ctx.debug = false;
    ctx.mount_mem(0x19E000, vec![0u8; 4 * 1024]);
    ctx.mount_mem(0x0070FC81, read_code());
    ctx.mount_mem(0xEF14F28, vec![0u8; 100]);
    ctx.mount_mem(0xF8200A0, vec![0u8; 1024 * 1024]);
    ctx.mount_mem_file(0x3BD3020, "island_gelato_indexbuffer.txt");
    ctx.set_register(Register::EIP, 0x0070FC81);
    ctx.set_register(Register::ESP, 0x19E074);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x10), 0x0);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x14), 0xC8);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x18), 0x1);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x1C), 0x0);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x20), 0x0);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x24), 0xEF14F28);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x38), 0x3BD3020);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x40), 0x0);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x1C), 0x2622); // 0EF14F44
    ctx.set_value(Value::Memory(0xEF14F28 + 0x20), 0x2);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x24), 0xF8200A0);
    ctx.set_value(Value::Memory(0xF8200A0 + 0x00), 0x00000C8);

    ctx.break_if_eip(0x0070FEA1);

    run(&mut ctx, usize::MAX - 1); //135

    let t2 = std::time::Instant::now();
    println!("took {:?}", t2 - t1);

    // for i in 0..0x2622 {
    //     print!("{:02X?} ", ctx.read_at::<u8>(0xF8200A0 + i));
    // }
    // println!("");

    // compare
    let expected = include_str!("../expected.txt");
    let expected = expected.as_bytes();

    let mut r = String::new();
    let output = ctx.borrow_mem(0xF8200A0);
    for i in 0..expected.len() {
        let s = format!("{:02X?} ", output[i]);
        r.push_str(s.as_str());
    }
    let r = r.as_bytes();

    let mut i = 0;
    loop {
        match (expected.get(i as usize), r.get(i as usize)) {
            (Some(l), Some(r)) if l == r => {
                i += 1;
            }
            (Some(l), Some(r)) if l != r => {
                println!("{}: {} != {}", i, *l as char, *r as char);
                break;
            }
            _ => break,
        }
    }
    println!("{} of {}", i, expected.len());
}
