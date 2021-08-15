use super::asm::*;
use super::elf::*;
use super::*;

#[extension_trait::extension_trait(pub)]
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
