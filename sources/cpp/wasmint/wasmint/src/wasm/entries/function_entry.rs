use crate::utils::*;
use crate::wasm::*;

#[derive(Debug)]
pub struct FunctionEntry
{
    pub tidx: u32,
}
impl FunctionEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let (tidx,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;
        Ok(FunctionEntry{ tidx })
    }
}
