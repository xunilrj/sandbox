use super::*;

#[extension_trait::extension_trait(pub)]
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
