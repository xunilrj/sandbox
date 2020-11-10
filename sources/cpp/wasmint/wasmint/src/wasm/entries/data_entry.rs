use crate::utils::*;
use crate::wasm::*;

#[derive(Debug)]
pub struct DataEntry
{
    pub index: u32,
    offset: FunctionBody,
    pub data: Vec<u8>
}
impl DataEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let (index,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;
        let offset = FunctionBody::from(f)?;
        let (size,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;

        let mut data = vec![0; size as usize];
        f.read_exact(&mut data);

        Ok(DataEntry{ index, offset, data })
    }
    
    pub fn get_offset(&self) -> Option<usize>
    {
        match self.offset.instructions[0] {
            OpCode::I32Const(v) => Some(v as usize),
            _ => None
        }
    }
}
