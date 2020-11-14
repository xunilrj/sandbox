use std::fmt::Debug;
use crate::utils::*;
use crate::wasm::*;

#[derive(Debug)]
pub struct MemoryEntry
{
    pub initial: u32,
    pub maximum: Option<u32>,
}
impl MemoryEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let (flags,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;
        let (initial,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;
        
        let mut maximum = None;
        if flags == 1 {
            let (max,_) = read_uleb128(f)
                .map_err(WasmError::IOError)?;
            maximum = Some(max);
        }

        Ok(MemoryEntry{ initial, maximum })
    }
}