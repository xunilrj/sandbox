use super::*;

#[extension_trait::extension_trait(pub)]
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
