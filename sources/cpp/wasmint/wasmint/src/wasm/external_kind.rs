use crate::utils::*;
use super::wasm_error::*;

#[derive(Debug)]
pub enum ExternalKind
{
    Function(u32),
    Table(u32),
    Memory(u32),
    Global(u32),
}

impl ExternalKind
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<ExternalKind, WasmError>
    {
        let kind = read_u8(f)
                .map_err(WasmError::IOError)?;

        match kind {
            0 => {
                let (t,_) = read_uleb128(f).map_err(WasmError::IOError)?;
                Ok(ExternalKind::Function(t))
            },
            1 => {
                let (t,_) = read_uleb128(f).map_err(WasmError::IOError)?;
                Ok(ExternalKind::Table(t))
            },
            2 => {
                let (t,_) = read_uleb128(f).map_err(WasmError::IOError)?;
                Ok(ExternalKind::Memory(t))
            },
            3 => {
                let (t,_) = read_uleb128(f).map_err(WasmError::IOError)?;
                Ok(ExternalKind::Global(t))
            },
            _ => Err(WasmError::UnkownKind)
        }
    }
}