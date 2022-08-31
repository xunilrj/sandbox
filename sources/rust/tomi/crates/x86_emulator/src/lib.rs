use iced_x86::{
    Code, Decoder, Formatter, Instruction, InstructionInfoFactory, Mnemonic, NasmFormatter, OpKind,
    Register,
};
use rust_lapper::{Interval, Lapper};
use std::{
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet},
    io::{stdout, Write},
};

fn high_u8(v: u32) -> u8 {
    ((v & 0xFF00) >> 8) as u8
}

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
    pf: bool,
    c0: bool,
    c1: bool,
    c2: bool,
    c3: bool,
    eip: u32,
    esi: u32,
    esp: u32,
    ebp: u32,
    edi: u32,
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
    float_stack: Vec<f32>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            debug: false,
            of: false,
            sf: false,
            cf: false,
            zf: false,
            pf: false,
            c0: false,
            c1: false,
            c2: false,
            c3: false,
            eip: 0,
            esi: 0,
            esp: 0,
            ebp: 0,
            eax: 0,
            ebx: 0,
            ecx: 0,
            edx: 0,
            edi: 0,
            registers: HashMap::new(),
            intervals: vec![],
            mem: Lapper::new(vec![]),
            mems: vec![],
            log: String::new(),
            eip_breaker: 0xFFFFFFFF,
            float_stack: vec![],
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

    #[allow(dead_code)]
    pub fn mount_mem_file(&mut self, addr: usize, path: &str) {
        let v = std::fs::read(path).unwrap();
        self.mount_mem(addr, v)
    }

    pub fn get_register(&mut self, register: iced_x86::Register) -> Value {
        match register {
            Register::None => return Value::U32(0),
            Register::EIP => return Value::U32(self.eip),
            Register::EAX => return Value::U32(self.eax),
            Register::EBX => return Value::U32(self.ebx),
            Register::ECX => return Value::U32(self.ecx),
            Register::EDX => return Value::U32(self.edx),
            Register::ESI => return Value::U32(self.esi),
            Register::EBP => return Value::U32(self.ebp),
            Register::EDI => return Value::U32(self.edi),
            Register::ESP => return Value::U32(self.esp),
            _ => {}
        }

        let name = format!("{:?}", register);
        if let Some(r) = self.registers.get(&name) {
            if self.debug {
                self.log
                    .push_str(format!(" read {}=0x{:X?}", name, r).as_str());
            }
            Value::U32(*r)
        } else {
            let v = match name.as_str() {
                "AX" => {
                    let v = self.get_register(Register::EAX);
                    Value::U16(low_u16(v.as_u32()))
                }
                "AH" => {
                    let v = self.get_register(Register::EAX);
                    Value::U8(high_u8(v.as_u32()))
                }
                "DX" => {
                    let v = self.get_register(Register::EDX);
                    Value::U16(low_u16(v.as_u32()))
                }
                "CL" => {
                    let v = self.get_register(Register::ECX);
                    Value::U8(low_u8(v.as_u32()))
                }
                x => todo!("{:?}", x),
            };

            if self.debug {
                self.log
                    .push_str(format!(" read {}={:X?}", name, v).as_str());
            }

            v
        }
    }

    pub fn set_register(&mut self, register: iced_x86::Register, v: u32) {
        let register = match register {
            Register::AH => Register::EAX,
            Register::AL => Register::EAX,
            Register::BH => Register::EBX,
            Register::BL => Register::EBX,
            Register::CH => Register::ECX,
            Register::CL => Register::ECX,
            Register::DH => Register::EDX,
            Register::DL => Register::EDX,
            _ => register,
        };

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
            Register::ESI => {
                self.esi = v;
                if self.debug {
                    self.log.push_str(format!(" write ESI=0x{:X?}", v).as_str());
                }
                return;
            }
            Register::EBP => {
                self.ebp = v;
                if self.debug {
                    self.log.push_str(format!(" write EBP=0x{:X?}", v).as_str());
                }
                return;
            }
            Register::EDI => {
                self.edi = v;
                if self.debug {
                    self.log.push_str(format!(" write EDI=0x{:X?}", v).as_str());
                }
                return;
            }
            Register::ESP => {
                self.esp = v;
                if self.debug {
                    self.log.push_str(format!(" write ESP=0x{:X?}", v).as_str());
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

    pub fn solve_u32(&mut self, addr: Value) -> Value {
        match addr {
            Value::MemoryRegisterValue(base, delta) => {
                let addr = self.get_register(base);
                Value::U32(self.read_at((addr + delta) as usize))
            }
            Value::Memory(addr) => Value::U32(self.read_at((addr) as usize)),
            Value::Register(r) => self.get_register(r),
            Value::U8(v) => Value::U8(v),
            Value::U16(v) => Value::U16(v),
            Value::U32(v) => Value::U32(v),
            Value::F32(_) => panic!("Impossible"),
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
            if self.debug {
                println!("Memory not found: {} 0x{:X?}", addr, addr);
            }
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

    pub fn set_value(&mut self, dst: Value, v: Value) {
        match dst {
            Value::Register(register) => self.set_register(register, v.as_u32()),
            Value::MemoryRegisterValue(base, delta) => {
                let base = self.get_register(base).as_usize();
                let addr = base + (delta as usize);
                let (start, mem) = self.get_mem_vec_mut(addr);
                let addr = addr - start;

                match v {
                    Value::U8(v) => {
                        let addr: &mut u8 = unsafe { std::mem::transmute(&mut mem[addr]) };
                        *addr = v;
                    }
                    Value::U16(v) => {
                        let addr: &mut u16 = unsafe { std::mem::transmute(&mut mem[addr]) };
                        *addr = v;
                    }
                    Value::U32(v) => {
                        let addr: &mut u32 = unsafe { std::mem::transmute(&mut mem[addr]) };
                        *addr = v;
                    }
                    Value::F32(v) => {
                        let addr: &mut f32 = unsafe { std::mem::transmute(&mut mem[addr]) };
                        *addr = v;
                    }
                    _ => todo!(),
                }

                if self.debug {
                    self.log.push_str(
                        format!(" write mem[0x{:X?}]={:X?}", base + (delta as usize), v).as_str(),
                    );
                }
            }
            Value::Memory(addr) => {
                let (start, mem) = self.get_mem_vec_mut(addr as usize);
                let addr = addr as usize - start;

                match v {
                    Value::U8(v) => {
                        let addr: &mut u8 = unsafe { std::mem::transmute(&mut mem[addr]) };
                        *addr = v;
                    }
                    Value::U16(v) => {
                        let addr: &mut u16 = unsafe { std::mem::transmute(&mut mem[addr]) };
                        *addr = v;
                    }
                    Value::U32(v) => {
                        let addr: &mut u32 = unsafe { std::mem::transmute(&mut mem[addr]) };
                        *addr = v;
                    }
                    _ => todo!(),
                }

                if self.debug {
                    self.log
                        .push_str(format!(" write mem[0x{:X?}]=0x{:X?}", addr, v).as_str());
                }
            }
            Value::U8(_) => panic!("Impossible"),
            Value::U16(_) => panic!("Impossible"),
            Value::U32(_) => panic!("Impossible"),
            Value::F32(_) => panic!("Impossible"),
        }
    }

    pub fn set_value_op0(&mut self, i: &Instruction, v: Value) {
        let addr = Value::from_op0(&i);
        self.set_value(addr, v);
    }

    pub fn read_op0_as_u32(&mut self, i: &Instruction) -> Value {
        self.solve_u32(Value::from_op0(&i))
    }

    pub fn read_op0_as_f32(&mut self, i: &Instruction) -> Value {
        let v = self.solve_u32(Value::from_op0(&i));
        match v {
            Value::U32(v) => {
                let bytes = v.to_le_bytes();
                Value::F32(f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
            }
            Value::F32(v) => Value::F32(v),
            _ => panic!("impossible"),
        }
    }

    pub fn read_op1(&mut self, i: &Instruction) -> Value {
        self.solve_u32(Value::from_op1(&i))
    }

    pub fn read_op0_1(&mut self, i: &Instruction) -> (Value, Value) {
        (
            self.solve_u32(Value::from_op0(&i)),
            self.solve_u32(Value::from_op1(&i)),
        )
    }

    pub fn register_incr(&mut self, register: iced_x86::Register, incr: isize) -> u32 {
        let v = self.get_register(register) + incr;
        self.set_register(register, v as u32);
        v as u32
    }

    pub fn read_eip(&mut self) -> (u32, &[u8]) {
        let eip = self.get_register(iced_x86::Register::EIP).as_usize();
        let (start, mem) = self.get_mem_vec(eip);
        let i = eip - start;
        let mem = mem.as_slice();
        (eip as u32, &mem[i..])
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

    pub fn push_stack(&mut self, v: u32) {
        let esp = self.get_register(iced_x86::Register::ESP).as_u32();
        self.set_value(Value::Memory(esp), Value::U32(v));
        self.register_incr(iced_x86::Register::ESP, -4);
    }

    pub fn push_f32(&mut self, v: Value) {
        let v = v.as_f32();

        if self.debug {
            self.log.push_str(format!(" pushed ST0={:?}", v).as_str());
        }

        self.float_stack.push(v);
    }

    pub fn pop_f32(&mut self) -> Value {
        let v = self.float_stack.pop().unwrap();

        if self.debug {
            self.log.push_str(format!(" pop ST0={:?}", v).as_str());
        }

        Value::F32(v)
    }

    pub fn peek_f32(&mut self) -> Value {
        let v = self.float_stack.last().unwrap();
        Value::F32(*v)
    }

    pub fn compare_floats(&mut self, a: f32, b: f32) {
        // Condition	C3	C2	C0
        // ST(0) > SRC	0	0	0
        // ST(0) < SRC	0	0	1
        // ST(0) = SRC	1	0	0
        // Unordered*	1	1	1
        match a.partial_cmp(&b) {
            Some(Ordering::Greater) => {
                self.c0 = false;
                self.c2 = false;
                self.c3 = false;
            }
            Some(Ordering::Less) => {
                self.c0 = true;
                self.c2 = false;
                self.c3 = false;
            }
            Some(Ordering::Equal) => {
                self.c0 = false;
                self.c2 = false;
                self.c3 = true;
            }
            None => {
                self.c0 = true;
                self.c2 = true;
                self.c3 = true;
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Register(iced_x86::Register),
    Memory(u32),
    MemoryRegisterValue(iced_x86::Register, u32),
    U8(u8),
    U16(u16),
    U32(u32),
    F32(f32),
}

impl std::ops::Add<isize> for Value {
    type Output = isize;

    fn add(self, rhs: isize) -> Self::Output {
        match &self {
            Value::U8(x) => (*x as isize) + rhs,
            Value::U16(x) => (*x as isize) + rhs,
            Value::U32(x) => (*x as isize) + rhs,
            _ => todo!(),
        }
    }
}

impl std::ops::Add<u32> for Value {
    type Output = u32;

    fn add(self, rhs: u32) -> Self::Output {
        match &self {
            Value::U8(x) => (*x as u32) + rhs,
            Value::U16(x) => (*x as u32) + rhs,
            Value::U32(x) => {
                let (r, overflow) = (*x as u32).overflowing_add(rhs);
                r
            }
            _ => todo!(),
        }
    }
}

impl std::ops::BitAnd for Value {
    type Output = Value;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::U8(l), Value::U8(r)) => Value::U8(l & r),
            (Value::U16(l), Value::U16(r)) => Value::U16(l & r),
            (Value::U32(l), Value::U32(r)) => Value::U32(l & r),
            _ => todo!(),
        }
    }
}

impl std::ops::BitOr for Value {
    type Output = Value;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::U8(l), Value::U8(r)) => Value::U8(l | r),
            (Value::U16(l), Value::U16(r)) => Value::U16(l | r),
            (Value::U32(l), Value::U32(r)) => Value::U32(l | r),
            _ => todo!(),
        }
    }
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
            iced_x86::OpKind::Immediate16 => Value::U16(i.immediate16()),
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

    pub fn as_u32(&self) -> u32 {
        match &self {
            Value::U8(x) => *x as u32,
            Value::U16(x) => *x as u32,
            Value::U32(x) => *x as u32,
            x => todo!("{:?}", x),
        }
    }

    pub fn as_i64(&self) -> i64 {
        match &self {
            Value::U8(x) => *x as i64,
            Value::U16(x) => *x as i64,
            Value::U32(x) => *x as i64,
            _ => todo!(),
        }
    }

    pub fn as_usize(&self) -> usize {
        match &self {
            Value::U8(x) => *x as usize,
            Value::U16(x) => *x as usize,
            Value::U32(x) => *x as usize,
            _ => todo!(),
        }
    }

    pub fn as_isize(&self) -> isize {
        match &self {
            Value::U8(x) => *x as isize,
            Value::U16(x) => *x as isize,
            Value::U32(x) => *x as isize,
            _ => todo!(),
        }
    }

    pub fn as_f32(&self) -> f32 {
        match &self {
            Value::F32(x) => *x as f32,
            _ => todo!(),
        }
    }

    pub fn overflowing_shr(&self, rhs: Value) -> (Value, bool) {
        let rhs = rhs.as_u32();
        match &self {
            Value::U8(x) => {
                let (x, cf) = x.overflowing_shr(rhs);
                (Value::U8(x), cf)
            }
            Value::U16(x) => {
                let (x, cf) = x.overflowing_shr(rhs);
                (Value::U16(x), cf)
            }
            Value::U32(x) => {
                let (x, cf) = x.overflowing_shr(rhs);
                (Value::U32(x), cf)
            }
            _ => todo!(),
        }
    }

    pub fn overflowing_shl(&self, rhs: Value) -> (Value, bool) {
        let rhs = rhs.as_u32();
        match &self {
            Value::U8(x) => {
                let (x, cf) = x.overflowing_shl(rhs);
                (Value::U8(x), cf)
            }
            Value::U16(x) => {
                let (x, cf) = x.overflowing_shl(rhs);
                (Value::U16(x), cf)
            }
            Value::U32(x) => {
                let (x, cf) = x.overflowing_shl(rhs);
                (Value::U32(x), cf)
            }
            _ => todo!(),
        }
    }

    pub fn overflowing_mul(&self, rhs: Value) -> (Value, bool) {
        match (&self, rhs) {
            (Value::U8(l), Value::U8(r)) => {
                let (x, cf) = (*l as i64).overflowing_mul(r as i64);
                (Value::U8(x as u8), cf)
            }
            (Value::U16(l), Value::U16(r)) => {
                let (x, cf) = (*l as i64).overflowing_mul(r as i64);
                (Value::U16(x as u16), cf)
            }
            (Value::U32(l), Value::U32(r)) => {
                let (x, cf) = (*l as i64).overflowing_mul(r as i64);
                (Value::U32(x as u32), cf)
            }
            _ => todo!(),
        }
    }

    pub fn overflowing_add(&self, rhs: Value) -> (Value, bool) {
        match (&self, rhs) {
            (Value::U8(l), Value::U8(r)) => {
                let (x, cf) = l.overflowing_add(r);
                (Value::U8(x as u8), cf)
            }
            (Value::U16(l), Value::U16(r)) => {
                let (x, cf) = l.overflowing_add(r);
                (Value::U16(x as u16), cf)
            }
            (Value::U32(l), Value::U32(r)) => {
                let (x, cf) = l.overflowing_add(r);
                (Value::U32(x as u32), cf)
            }
            _ => todo!(),
        }
    }

    pub fn overflowing_sub(&self, rhs: Value) -> (Value, bool) {
        match (&self, rhs) {
            (Value::U8(l), Value::U8(r)) => {
                let (x, cf) = l.overflowing_sub(r);
                (Value::U8(x as u8), cf)
            }
            (Value::U16(l), Value::U16(r)) => {
                let (x, cf) = l.overflowing_sub(r);
                (Value::U16(x as u16), cf)
            }
            (Value::U32(l), Value::U32(r)) => {
                let (x, cf) = l.overflowing_sub(r);
                (Value::U32(x as u32), cf)
            }
            _ => todo!(),
        }
    }
}

pub fn code_from_hex_string(hex_string: &str) -> Vec<u8> {
    let mut byte = String::new();
    let mut code = vec![];

    for chr in hex_string.chars() {
        if chr.is_whitespace() {
            continue;
        }

        byte.push(chr);

        if byte.len() == 2 {
            let v = u8::from_str_radix(&byte, 16).unwrap();
            code.push(v);
            byte.clear();
        }
    }

    debug_assert!(byte.len() == 0);

    code
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

        if eip == breaker {
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
            let _ = stdout().flush();
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
                ctx.zf = v.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Or => {
                let (dst, src) = ctx.read_op0_1(&i);
                let v = dst | src;
                ctx.set_value_op0(&i, v);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = false;
                ctx.zf = v.as_isize() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Add => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_add(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Inc => {
                let v = ctx.read_op0_as_u32(&i);
                let (wrapped_value, cf) = v.overflowing_add(Value::U32(1));
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Cmc => {
                ctx.cf = !ctx.cf;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Sub => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_sub(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Imul => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, of) = dst.overflowing_mul(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = of;
                ctx.sf = wrapped_value.as_i64() < 0;
                ctx.cf = false;
                ctx.zf = wrapped_value.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Shl => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_shl(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Shr => {
                let (dst, src) = ctx.read_op0_1(&i);
                let (wrapped_value, cf) = dst.overflowing_shr(src);
                ctx.set_value_op0(&i, wrapped_value);
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = cf;
                ctx.zf = wrapped_value.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Neg => {
                //how neg works on 16, 8 registers?
                let v = 0isize - ctx.read_op0_as_u32(&i).as_isize();
                let v = v as u32;
                ctx.set_value_op0(&i, Value::U32(v));
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
                ctx.zf = wrapped_value.as_i64() == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Test => {
                let (dst, src) = ctx.read_op0_1(&i);
                let v = dst & src;
                ctx.of = false;
                ctx.sf = false;
                ctx.cf = false;
                ctx.zf = v.as_i64() == 0;
                ctx.pf = v.as_u32().count_ones() % 2 == 0;
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Jmp => {
                let addr = ctx.read_op0_as_u32(&i) + eip;
                ctx.set_register(iced_x86::Register::EIP, addr as u32);
            }
            iced_x86::Mnemonic::Je => {
                if ctx.zf {
                    let addr = ctx.read_op0_as_u32(&i) + eip;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jne => {
                if !ctx.zf {
                    let addr = ctx.read_op0_as_u32(&i) + eip;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jae => {
                if !ctx.cf || ctx.zf {
                    let addr = ctx.read_op0_as_u32(&i) + eip;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jle => {
                if ctx.sf != ctx.of || ctx.zf {
                    let addr = ctx.read_op0_as_u32(&i) + eip;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jl => {
                if ctx.sf != ctx.of {
                    let addr = ctx.read_op0_as_u32(&i) + eip;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Jp => {
                if ctx.pf {
                    let addr = ctx.read_op0_as_u32(&i) + eip;
                    ctx.set_register(iced_x86::Register::EIP, addr as u32);
                } else {
                    ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
                }
            }
            iced_x86::Mnemonic::Push => {
                ctx.register_incr(iced_x86::Register::ESP, -4);

                let v = ctx.read_op0_as_u32(&i);
                let esp = ctx.get_register(iced_x86::Register::ESP).as_u32();
                ctx.set_value(Value::Memory(esp), v);

                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Pop => {
                let addr = ctx.get_register(iced_x86::Register::ESP).as_u32();
                let v = ctx.read_at(addr as usize);
                ctx.set_value_op0(&i, Value::U32(v));
                ctx.register_incr(iced_x86::Register::ESP, 4);
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Fld => {
                let v = ctx.read_op0_as_f32(&i);
                ctx.push_f32(v);
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Fldz => {
                ctx.push_f32(Value::F32(0.0));
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Fstp => {
                let v = ctx.pop_f32();
                ctx.set_value_op0(&i, v);
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Fst => {
                let v = ctx.peek_f32();
                ctx.set_value_op0(&i, v);
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Fcomp => {
                let (a, b) = match i.op_count() {
                    0 => {
                        let st0 = ctx.pop_f32();
                        let st1 = ctx.peek_f32();
                        (st0, st1)
                    }
                    2 => {
                        let st0 = ctx.pop_f32();
                        let b = match i.op1_register() {
                            Register::ST1 => ctx.peek_f32(),
                            _ => todo!(),
                        };
                        (st0, b)
                    }
                    _ => todo!(),
                };

                ctx.compare_floats(a.as_f32(), b.as_f32());
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Fcompp => {
                let (a, b) = match i.op_count() {
                    0 => {
                        let st0 = ctx.pop_f32();
                        let st1 = ctx.pop_f32();
                        (st0, st1)
                    }
                    2 => {
                        let st0 = ctx.pop_f32();
                        let b = match i.op1_register() {
                            Register::ST1 => ctx.pop_f32(),
                            _ => todo!(),
                        };
                        (st0, b)
                    }
                    _ => todo!(),
                };

                ctx.compare_floats(a.as_f32(), b.as_f32());
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Fnstsw => {
                let c0 = if ctx.c0 { 1u32 } else { 0u32 };
                let c1 = if ctx.c1 { 1u32 } else { 0u32 };
                let c2 = if ctx.c2 { 1u32 } else { 0u32 };
                let c3 = if ctx.c3 { 1u32 } else { 0u32 };
                let status = c0 | (c1 << 1) | (c2 << 2) | (c3 << 3);
                ctx.set_value_op0(&i, Value::U32(status));
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Call => {
                let addr = ctx.get_register(iced_x86::Register::EIP) + i.len() as isize;
                ctx.push_stack(addr as u32);

                let delta = ctx.read_op0_as_u32(&i).as_u32();
                ctx.register_incr(iced_x86::Register::EIP, delta as isize);
            }
            iced_x86::Mnemonic::Ret => {
                let size = ctx.read_op0_as_u32(&i).as_u32();
                ctx.register_incr(iced_x86::Register::ESP, size as isize);

                let addr = ctx.get_register(iced_x86::Register::ESP);
                ctx.set_register(iced_x86::Register::EIP, addr.as_u32());
            }
            iced_x86::Mnemonic::Lea => {
                let addr = match Value::from_op1(&i) {
                    Value::MemoryRegisterValue(base, delta) => {
                        let addr = ctx.get_register(base);
                        Value::U32(addr + delta)
                    }
                    _ => panic!(),
                };

                ctx.set_value_op0(&i, addr);
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            iced_x86::Mnemonic::Sets => {
                let v = if ctx.sf { 1 } else { 0 };
                ctx.set_value_op0(&i, Value::U8(v as u8));
                ctx.register_incr(iced_x86::Register::EIP, i.len() as isize);
            }
            x @ _ => {
                if ctx.debug {
                    println!(" {}", ctx.log);
                }
                todo!("{}, todo: {:?}", original_steps - steps as usize, x);
            }
        }

        if ctx.debug {
            println!(" {}", ctx.log);
        }
    }
}

#[test]
pub fn test_emulator() {
    // let t1 = std::time::Instant::now();

    // let mut ctx = Context::new();
    // ctx.debug = true;
    // ctx.mount_mem(0x19E000, vec![0u8; 4 * 1024]);
    // ctx.mount_mem(0x0070FC81, read_code());
    // ctx.mount_mem(0xEF14F28, vec![0u8; 100]);
    // ctx.mount_mem(0xF8200A0, vec![0u8; 1024 * 1024]);
    // ctx.mount_mem_file(0x3BD3020, "island_gelato_indexbuffer.txt");
    // ctx.set_register(Register::EIP, 0x0070FC81);
    // ctx.set_register(Register::ESP, 0x19E074);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x10), 0x0);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x14), 0xC8);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x18), 0x1);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x1C), 0x0);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x20), 0x0);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x24), 0xEF14F28);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x38), 0x3BD3020);
    // ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x40), 0x0);
    // ctx.set_value(Value::Memory(0xEF14F28 + 0x1C), 0x2622); // 0EF14F44
    // ctx.set_value(Value::Memory(0xEF14F28 + 0x20), 0x2);
    // ctx.set_value(Value::Memory(0xEF14F28 + 0x24), 0xF8200A0);
    // ctx.set_value(Value::Memory(0xF8200A0 + 0x00), 0x00000C8);

    // ctx.break_if_eip(0x0070FEA1);

    // run(&mut ctx, usize::MAX - 1); //135

    // let t2 = std::time::Instant::now();
    // println!("took {:?}", t2 - t1);

    // // for i in 0..0x2622 {
    // //     print!("{:02X?} ", ctx.read_at::<u8>(0xF8200A0 + i));
    // // }
    // // println!("");

    // // compare
    // let expected = include_str!("../expected.txt");
    // let expected = expected.as_bytes();

    // let mut r = String::new();
    // let output = ctx.borrow_mem(0xF8200A0);
    // for i in 0..expected.len() {
    //     let s = format!("{:02X?} ", output[i]);
    //     r.push_str(s.as_str());
    // }
    // let r = r.as_bytes();

    // let mut i = 0;
    // loop {
    //     match (expected.get(i as usize), r.get(i as usize)) {
    //         (Some(l), Some(r)) if l == r => {
    //             i += 1;
    //         }
    //         (Some(l), Some(r)) if l != r => {
    //             println!("{}: {} != {}", i, *l as char, *r as char);
    //             break;
    //         }
    //         _ => break,
    //     }
    // }
    // println!("{} of {}", i, expected.len());
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Location {
    Register(iced_x86::Register),
    Memory(u64),
    RegisterOffset(iced_x86::Register, u64),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    U32,
    F32,
    Pointer(Box<Type>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Input {
    Argument(String, Type),
    Local(String, Type),
}

pub fn disassembly(code: &[u8]) {
    let mut written = HashSet::new();
    written.insert(Location::Register(Register::ESP));
    written.insert(Location::Register(Register::DS));
    written.insert(Location::Register(Register::SS));
    let mut read = HashSet::new();
    let mut read_before_written = HashSet::new();

    let mut info_factory = InstructionInfoFactory::new();
    let mut decoder = Decoder::new(32, code, 0);

    let mut st_index = 0;

    loop {
        if !decoder.can_decode() {
            break;
        }

        let mut i = Instruction::default();
        decoder.decode_out(&mut i);

        let info = info_factory.info(&i);

        println!("{:016X} {}", i.ip(), i);
        dbg!(&info);

        match i.mnemonic() {
            Mnemonic::Fld | Mnemonic::Fldz => {
                written.insert(Location::Register(Register::ST0 + st_index));
                st_index += 1;
            }
            Mnemonic::Fstp | Mnemonic::Fcomp => {
                written.remove(&(Location::Register(Register::ST0 + st_index)));
                st_index -= 1;
            }
            Mnemonic::Fcompp => {
                st_index -= 2;
            }
            _ => {}
        }

        for reg in info.used_registers() {
            let r = reg.register();
            let r = r.full_register32();
            let r = Location::Register(r);

            match reg.access() {
                iced_x86::OpAccess::None => {}
                iced_x86::OpAccess::Read => {
                    // if the first read is a push, we assume
                    // it is not a input
                    if !read.contains(&r) && i.mnemonic() == Mnemonic::Push {
                        written.insert(r);
                    }
                    read.insert(r);

                    if !written.contains(&r) {
                        read_before_written.insert((r, i.mnemonic()));
                    }
                }
                iced_x86::OpAccess::CondRead => {
                    // if the first read is a push, we assume
                    // it is not a input
                    if !read.contains(&r) && i.mnemonic() == Mnemonic::Push {
                        written.insert(r);
                    }
                    read.insert(r);

                    if !written.contains(&r) {
                        read_before_written.insert((r, i.mnemonic()));
                    }
                }
                iced_x86::OpAccess::Write => {
                    written.insert(r);
                }
                iced_x86::OpAccess::CondWrite => {
                    written.insert(r);
                }
                iced_x86::OpAccess::ReadWrite => {
                    // if the first read is a push, we assume
                    // it is not a input
                    if !read.contains(&r) && i.mnemonic() == Mnemonic::Push {
                        written.insert(r);
                    }
                    read.insert(r);

                    if !written.contains(&r) {
                        read_before_written.insert((r, i.mnemonic()));
                    }

                    written.insert(r);
                }
                iced_x86::OpAccess::ReadCondWrite => {
                    // if the first read is a push, we assume
                    // it is not a input
                    if !read.contains(&r) && i.mnemonic() == Mnemonic::Push {
                        written.insert(r);
                    }
                    read.insert(r);

                    if !written.contains(&r) {
                        read_before_written.insert((r, i.mnemonic()));
                    }

                    written.insert(r);
                }
                iced_x86::OpAccess::NoMemAccess => {}
            }
        }

        for mem in info.used_memory() {
            let location = match mem.base() {
                Register::None => Location::Memory(mem.displacement()),
                _ => Location::RegisterOffset(mem.base(), mem.displacement()),
            };
            match mem.access() {
                iced_x86::OpAccess::None => {}
                iced_x86::OpAccess::Read => {
                    if !written.contains(&location) {
                        read_before_written.insert((location, i.mnemonic()));
                    }
                }
                iced_x86::OpAccess::CondRead => {
                    if !written.contains(&location) {
                        read_before_written.insert((location, i.mnemonic()));
                    }
                }
                iced_x86::OpAccess::Write => {
                    written.insert(location);
                }
                iced_x86::OpAccess::CondWrite => {
                    written.insert(location);
                }
                iced_x86::OpAccess::ReadWrite => {
                    if !written.contains(&location) {
                        read_before_written.insert((location, i.mnemonic()));
                    }
                    written.insert(location);
                }
                iced_x86::OpAccess::ReadCondWrite => {
                    if !written.contains(&location) {
                        read_before_written.insert((location, i.mnemonic()));
                    }
                    written.insert(location);
                }
                iced_x86::OpAccess::NoMemAccess => {}
            }
        }
    }

    let mut inputs = HashSet::new();

    for (location, mnemonic) in read_before_written {
        let t = match mnemonic {
            Mnemonic::Fld
            | Mnemonic::Fst
            | Mnemonic::Fstp
            | Mnemonic::Fcom
            | Mnemonic::Fcomp
            | Mnemonic::Fcompp => Type::Pointer(Box::new(Type::F32)),
            _ => Type::Pointer(Box::new(Type::U32)),
        };
        match location {
            Location::RegisterOffset(Register::ESP, offset) => {
                let input = if offset > 0xec {
                    let name = format!("arg_{:X}", offset);
                    Input::Argument(name, t)
                } else {
                    let name = format!("local_{:X}", offset);
                    Input::Local(name, t)
                };

                inputs.insert(input);
            }
            _ => {}
        }
    }

    // dbg!(read_before_written);
    dbg!(inputs);
}
