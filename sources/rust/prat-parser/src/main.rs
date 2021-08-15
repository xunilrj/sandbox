#![feature(box_syntax)]

use std::io::Write;

use binarywriter::{BinaryWriter, ByteValue};
mod binarywriter;
mod pratparser;

use binarywriter::elf::*;
use binarywriter::os::*;
use binarywriter::*;

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

    //TODO chmod +x a.out
}
