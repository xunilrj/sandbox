#![feature(box_syntax)]
#[macro_use]
extern crate extension_trait;

use std::collections::HashMap;
use std::{fmt, io::Write};

enum S {
    Atom(String),
    Cons(String, Vec<S>),
}

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Atom(char),
    Op(char),
    Eof,
}

struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        let mut tokens = input
            .chars()
            .filter(|it| !it.is_ascii_whitespace())
            .map(|c| match c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' => Token::Atom(c),
                _ => Token::Op(c),
            })
            .collect::<Vec<_>>();
        tokens.reverse();
        Lexer { tokens }
    }

    fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }
    fn peek(&mut self) -> Token {
        self.tokens.last().copied().unwrap_or(Token::Eof)
    }
}

fn expr(input: &str) -> S {
    let mut lexer = Lexer::new(input);
    expr_bp(&mut lexer, 0)
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> S {
    let mut lhs = match lexer.next() {
        Token::Atom(it) => S::Atom(it.to_string()),
        Token::Op('(') => {
            let lhs = expr_bp(lexer, 0);
            assert_eq!(lexer.next(), Token::Op(')'));
            lhs
        }
        Token::Op(op) => {
            let ((), r_bp) = prefix_binding_power(op);
            let rhs = expr_bp(lexer, r_bp);
            S::Cons(op.to_string(), vec![rhs])
        }
        t => panic!("bad token: {:?}", t),
    };

    loop {
        let op = match lexer.peek() {
            Token::Eof => break,
            Token::Op(op) => op,
            t => panic!("bad token: {:?}", t),
        };

        if let Some((l_bp, ())) = postfix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            lhs = match op {
                '[' => {
                    let rhs = expr_bp(lexer, 0);
                    assert_eq!(lexer.next(), Token::Op(']'));
                    S::Cons(op.to_string(), vec![lhs, rhs])
                }
                '(' => {
                    let mut args = vec![expr_bp(lexer, 0)];
                    while lexer.peek() == Token::Op(',') {
                        lexer.next();
                        let arg = expr_bp(lexer, 0);
                        args.push(arg);
                    }
                    assert_eq!(lexer.next(), Token::Op(')'));
                    let mut v = vec![lhs];
                    v.append(&mut args);
                    S::Cons("invoke".to_string(), v)
                }
                _ => S::Cons(op.to_string(), vec![lhs]),
            };

            continue;
        }

        if let Some((l_bp, r_bp)) = infix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            lhs = if op == '?' {
                let mhs = expr_bp(lexer, 0);
                assert_eq!(lexer.next(), Token::Op(':'));
                let rhs = expr_bp(lexer, r_bp);
                S::Cons(op.to_string(), vec![lhs, mhs, rhs])
            } else {
                let rhs = expr_bp(lexer, r_bp);
                S::Cons(op.to_string(), vec![lhs, rhs])
            };
            continue;
        }

        break;
    }

    lhs
}

fn prefix_binding_power(op: char) -> ((), u8) {
    match op {
        '+' | '-' => ((), 9),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: char) -> Option<(u8, ())> {
    let res = match op {
        '!' => (11, ()),
        '[' | '(' => (11, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: char) -> Option<(u8, u8)> {
    let res = match op {
        '=' => (2, 1),
        '?' => (4, 3),
        '+' | '-' => (5, 6),
        '*' | '/' => (7, 8),
        '.' => (14, 13),
        _ => return None,
    };
    Some(res)
}

#[test]
fn tests() {
    let s = expr("1");
    assert_eq!(s.to_string(), "1");

    let s = expr("1 + 2 * 3");
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

    let s = expr("a + b * c * d + e");
    assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");

    let s = expr("f . g . h");
    assert_eq!(s.to_string(), "(. f (. g h))");

    let s = expr(" 1 + 2 + f . g . h * 3 * 4");
    assert_eq!(s.to_string(), "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))",);

    let s = expr("--1 * 2");
    assert_eq!(s.to_string(), "(* (- (- 1)) 2)");

    let s = expr("--f . g");
    assert_eq!(s.to_string(), "(- (- (. f g)))");

    let s = expr("-9!");
    assert_eq!(s.to_string(), "(- (! 9))");

    let s = expr("f . g !");
    assert_eq!(s.to_string(), "(! (. f g))");

    let s = expr("(((0)))");
    assert_eq!(s.to_string(), "0");

    let s = expr("x[0][1]");
    assert_eq!(s.to_string(), "([ ([ x 0) 1)");

    let s = expr(
        "a ? b :
         c ? d
         : e",
    );
    assert_eq!(s.to_string(), "(? a b (? c d e))");

    let s = expr("a = 0 ? b : c = d");
    assert_eq!(s.to_string(), "(= a (= (? 0 b c) d))");

    let s = expr("a(b)");
    assert_eq!(s.to_string(), "(invoke a b)");

    let s = expr("a(b,c)");
    assert_eq!(s.to_string(), "(invoke a b c)");

    let s = expr("b = a(b,1 + 1)");
    assert_eq!(s.to_string(), "(= b (invoke a b (+ 1 1)))");
}

use bytes::BufMut;
use fmt::Debug;

#[extension_trait(pub)]
impl PutUTF8 for Vec<u8> {
    fn put_utf8(&mut self, txt: &str) {
        let bytes = txt.as_bytes();
        self.put_slice(bytes);
    }
}

pub struct BinaryWriter {
    pos: usize,
    data: Vec<ByteValue>,
    vars: HashMap<String, ByteValue>,
}
impl BinaryWriter {
    pub fn new(start: u64) -> Self {
        let mut s = Self {
            pos: 0,
            data: Vec::new(),
            vars: HashMap::new(),
        };

        s.vars.insert("$$".to_string(), ByteValue::U64(start));
        s.vars.insert("$".to_string(), ByteValue::U64(start));

        s
    }

    pub fn as_u64(&self, v: &ByteValue) -> u64 {
        match &v {
            ByteValue::U64(x) => *x,
            ByteValue::Var(size, x) => self.val_u64(x),
            ByteValue::Expr(pos, _size, f) => {
                // println!("EXPR {} {}", pos, size);
                let v = f(self, *pos as u64);
                self.as_u64(&v)
            }
            _ => panic!(format!("{:?}", v)),
        }
    }

    pub fn val_u64(&self, name: &str) -> u64 {
        let v = self.vars.get(name);
        if v.is_none() {
            println!("{:?}", name);
        }
        self.as_u64(v.unwrap())
    }

    pub fn put(&mut self, mut v: ByteValue) {
        let dd = self.val_u64("$$");
        let dollar = self.vars.get_mut("$").unwrap();
        *dollar = ByteValue::U64(dd + self.pos as u64);

        let _oldpos = dd + self.pos as u64;

        self.pos += match &mut v {
            ByteValue::U8(_x) => 1,
            ByteValue::U8Slice(x) => x.len(),
            ByteValue::U16(_x) => 2,
            ByteValue::U32(_x) => 4,
            ByteValue::U64(_x) => 8,
            ByteValue::UTF8(x) => x.as_bytes().len(),
            ByteValue::Var(size, _x) => *size / 8,
            ByteValue::Expr(pos, size, _x) => {
                *pos = dd as usize + self.pos as usize;
                *size / 8
            }
        };
        // println!("{} {:?}", oldpos, v);
        self.data.push(v);
    }

    pub fn set(&mut self, name: &str, mut v: ByteValue) {
        let dd = self.val_u64("$$") as usize;
        let dollar = self.vars.get_mut("$").unwrap();
        *dollar = ByteValue::U64((dd + self.pos) as u64);
        if let ByteValue::Expr(pos, _size, _f) = &mut v {
            *pos = dd + self.pos;
        }
        // println!("{} {:?}", (dd + self.pos) as u64, &v);
        self.vars.insert(name.to_string(), v);
    }

    pub fn debug_here(&self) {
        let d = self.val_u64("$");
        println!("{:#8x}", d);
    }

    pub fn set_as_here(&mut self, name: &str) {
        let d = self.val_u64("$");
        self.vars.insert(name.to_string(), ByteValue::U64(d));
    }

    pub fn set_as_distance(&mut self, name: &str, to: &str) {
        let to = to.to_string();
        self.set(
            name,
            ByteValue::Expr(0, 64, box move |x: &BinaryWriter, pos| {
                ByteValue::U64(pos - x.val_u64(to.as_str()))
            }),
        );
    }

    pub fn set_string_with_size(&mut self, name: &str, string: &str) {
        self.set_as_here(name);
        self.put(ByteValue::UTF8(string.to_string()));
        self.set_as_distance(&format!("{}_SIZE", name), name);
    }

    pub fn set_as_fileoffset(&mut self, name: &str) {
        let d = self.pos as u64;
        self.vars.insert(name.to_string(), ByteValue::U64(d));
    }

    fn write_to(&self, b: &ByteValue, v: &mut Vec<u8>) {
        match b {
            ByteValue::U8(x) => v.put_u8(*x),
            ByteValue::U8Slice(x) => {
                for b in x {
                    v.put_u8(*b);
                }
            }
            ByteValue::U16(x) => v.put_u16_le(*x),
            ByteValue::U32(x) => v.put_u32_le(*x),
            ByteValue::U64(x) => v.put_u64_le(*x),
            ByteValue::UTF8(x) => v.put_utf8(x),
            ByteValue::Var(size, x) => match *size {
                16 => {
                    let value = self.val_u64(x);
                    v.put_u16_le(value as u16);
                }
                32 => {
                    let value = self.val_u64(x);
                    v.put_u32_le(value as u32);
                }
                64 => {
                    let value = self.val_u64(x);
                    v.put_u64_le(value);
                }
                _ => panic!(format!("{}", size)),
            },
            ByteValue::Expr(pos, size, x) => {
                let value = x(self, *pos as u64);
                match *size {
                    16 => {
                        let value = self.as_u64(&value);
                        v.put_u16_le(value as u16);
                    }
                    64 => {
                        let value = self.as_u64(&value);
                        v.put_u64_le(value);
                    }
                    _ => panic!(""),
                }
            }
        }
    }

    pub fn as_vec(&mut self) -> Vec<u8> {
        let dd = self.val_u64("$$");
        self.vars.insert("$".to_string(), ByteValue::U64(dd));

        let mut v: Vec<u8> = Vec::new();
        for item in &self.data {
            // println!("{:?}", item);
            let dd = self.val_u64("$$");
            let dollar = self.vars.get_mut("$").unwrap();
            *dollar = ByteValue::U64(dd + v.len() as u64);

            self.write_to(&item, &mut v);
        }
        v
    }
}

pub enum ByteValue {
    U8(u8),
    U8Slice(Vec<u8>),
    U16(u16),
    U32(u32),
    U64(u64),

    UTF8(String),
    Var(usize, String),
    Expr(usize, usize, Box<dyn Fn(&BinaryWriter, u64) -> ByteValue>),
}
impl Debug for ByteValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match &self {
            ByteValue::U8(x) => f.write_fmt(format_args!("U8({})", x)),
            ByteValue::U8Slice(_x) => f.write_fmt(format_args!("U8Slice()")),
            ByteValue::U16(x) => f.write_fmt(format_args!("U16({})", x)),
            ByteValue::U32(x) => f.write_fmt(format_args!("U32({})", x)),
            ByteValue::U64(x) => f.write_fmt(format_args!("U64({})", x)),
            ByteValue::UTF8(x) => f.write_fmt(format_args!("UTF8({})", x)),
            ByteValue::Var(_size, x) => f.write_fmt(format_args!("Var({})", x)),
            ByteValue::Expr(pos, size, _x) => {
                f.write_fmt(format_args!("Expr({:?},{:?})", pos, size))
            }
        }
    }
}

#[extension_trait(pub)]
impl Elf for BinaryWriter {
    fn elf(&mut self) {
        use ByteValue::{Expr, U8Slice, Var, U16, U32, U64, U8, UTF8};
        self.set_as_here("ehdr");
        self.put(U8(0x7F));
        self.put(UTF8("ELF".to_string()));
        self.put(U8Slice(vec![2, 1, 1, 0]));
        self.put(U8Slice(vec![0; 8]));
        self.put(U16(2)); // e_type
        self.put(U16(0x3e)); // e_machine
        self.put(U32(1)); // e_version
        self.put(Var(64, "_start".to_string())); // e_entry
        self.put(Expr(0, 64, box |x, pos| {
            U64(x.val_u64("phdr") - x.val_u64("$$"))
        })); // e_phoff
        self.put(Var(64, "shdr".to_string())); // e_shoff
        self.put(U32(0));
        self.put(Var(16, "ehdrsize".to_string())); // e_ehsize
        self.put(Var(16, "phdrsize".to_string())); // e_phentsize
        self.put(U16(1)); // e_phnum
        self.put(U16(0x40)); // e_shentsize
        self.put(U16(2)); // e_shnum
        self.put(U16(0)); // e_shstrndx
        self.set(
            "ehdrsize",
            ByteValue::Expr(0, 64, box |x: &BinaryWriter, pos| {
                U64(pos - x.val_u64("ehdr"))
            }),
        );

        self.set_as_here("phdr");
        self.put(U32(1));
        self.put(U32(5));
        self.put(U64(0));
        self.put(Var(64, "$$".to_string())); // $$
        self.put(Var(64, "$$".to_string())); // $$
        self.put(Var(64, "filesize".to_string())); // filesize
        self.put(Var(64, "filesize".to_string())); // filesize
        self.put(U64(0x1000));
        self.set(
            "phdrsize",
            ByteValue::Expr(0, 64, box |x: &BinaryWriter, pos| {
                U64(pos - x.val_u64("phdr"))
            }),
        );
    }

    fn elf_filesize(&mut self) {
        use ByteValue::{Expr, U8Slice, Var, U16, U32, U64, U8, UTF8};

        self.set_as_fileoffset("shdr");
        self.put(U32(0x0000)); // sh_name
        self.put(U32(0x0003)); // sh_type
        self.put(U64(0x0020)); // sh_flags
        self.put(U64(0x0000)); // sh_addr
        self.put(Var(64, "shstrtab".to_string())); // sh_offset
        self.put(Var(64, "shstrtab_size".to_string())); // sh_size
        self.put(U32(0x0000)); // sh_link
        self.put(U32(0x0000)); // sh_info
        self.put(U64(0x0000)); // sh_addralign
        self.put(U64(0x40)); // sh_entsize
        self.put(U16(0x00)); // padding

        self.put(Var(32, ".text_name".to_string()));
        self.put(U32(1));
        self.put(U64(0x0004));
        self.put(U64(0x0000));
        self.put(Var(64, "_start".to_string()));
        self.put(Var(64, "code_size".to_string()));
        self.put(U32(0x0000));
        self.put(U32(0x0000));
        self.put(U64(0x0000));
        self.put(U64(0x40));
        self.put(U16(0x00)); // padding

        self.set_as_fileoffset("shstrtab");
        self.put(UTF8("section1name\0".to_string()));
        self.set(
            ".text_name",
            ByteValue::Expr(0, 64, box |x: &BinaryWriter, pos| {
                U64((pos - x.val_u64("$$")) - x.val_u64("shstrtab"))
            }),
        );
        self.put(UTF8(".text\0".to_string()));
        self.set(
            "shstrtab_size",
            ByteValue::Expr(0, 64, box |x: &BinaryWriter, pos| {
                U64((pos - x.val_u64("$$")) - x.val_u64("shstrtab"))
            }),
        );

        self.set(
            "filesize",
            ByteValue::Expr(
                0,
                64,
                box |x: &BinaryWriter, pos| U64(pos - x.val_u64("$$")),
            ),
        );
    }
}

#[extension_trait(pub)]
impl Asm for BinaryWriter {
    // mov eax,arg0
    fn op_mov_rax_u32(&mut self, arg0: ByteValue) {
        self.put(ByteValue::U8Slice(vec![0xb8]));
        self.put(arg0);
    }

    // mov edi,arg0
    fn op_mov_rdi_u32(&mut self, arg0: ByteValue) {
        self.put(ByteValue::U8Slice(vec![0xbf]));
        self.put(arg0);
    }

    // movabs rsi,arg0
    fn op_mov_rsi_u64(&mut self, arg0: ByteValue) {
        self.put(ByteValue::U8Slice(vec![0x48, 0xbe]));
        self.put(arg0);
    }

    // mov edx,0xd
    fn op_mov_rdx_u32(&mut self, arg0: ByteValue) {
        self.put(ByteValue::U8Slice(vec![0xba]));
        self.put(arg0);
    }

    fn op_syscall(&mut self) {
        self.put(ByteValue::U16(0x050F));
    }
}

#[extension_trait(pub)]
impl OS for BinaryWriter {
    fn os_exit(&mut self, code: u32) {
        self.op_mov_rax_u32(ByteValue::U32(231));
        self.op_mov_rdi_u32(ByteValue::U32(code));
        self.op_syscall();
    }

    fn os_print(&mut self, string: ByteValue, size: ByteValue) {
        self.op_mov_rax_u32(ByteValue::U32(1));
        self.op_mov_rdi_u32(ByteValue::U32(1));
        self.op_mov_rsi_u64(string);
        self.op_mov_rdx_u32(size);
        self.op_syscall();
    }
}

fn main() {
    let mut out = BinaryWriter::new(0x400000);
    out.elf();
    out.set_as_here("_start");
    out.os_print(
        ByteValue::Var(64, "HELLOWORLD".to_string()),
        ByteValue::Var(32, "HELLOWORLD_SIZE".to_string()),
    );
    out.os_exit(0);
    out.set(
        "code_size",
        ByteValue::Expr(0, 64, box |x: &BinaryWriter, pos| {
            ByteValue::U64(pos - x.val_u64("_start"))
        }),
    );
    out.set_string_with_size("HELLOWORLD", "Hello world!\0");
    out.elf_filesize();

    let mut fs = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open("a.out")
        .unwrap();
    let bytes = out.as_vec();
    fs.write_all(bytes.as_slice()).unwrap();
}
