pub enum Message {
    Buffer(Buffer),
    Eof,
}

pub struct Buffer {
    pub data: Vec<u8>,
    pub size: usize,
}

impl Buffer {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            size: 0,
        }
    }

    pub fn all_zero(size: usize) -> Self {
        Self {
            data: vec![0; size],
            size: size,
        }
    }

    pub fn as_u8(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.size) }
    }

    pub fn push_u8(&mut self, value: u8) {
        self.data.push(value);
        self.size += 1;
    }
}
