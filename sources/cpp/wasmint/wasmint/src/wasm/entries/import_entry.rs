use crate::utils::*;
use super::super::*;

#[derive(Debug)]
pub struct ImportEntry
{
    module: String,
    pub field: String,
    pub kind: ExternalKind,
}

impl ImportEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let (module,_) = read_uleb128_string(f).map_err(WasmError::IOError)?;
        let (field,_) = read_uleb128_string(f).map_err(WasmError::IOError)?;

        Ok(ImportEntry{ module, field, kind: ExternalKind::from(f)? })
    }

    pub fn get_function_type(&self) -> Option<usize>
    {
        match self.kind {
            ExternalKind::Function(f) => Some(f as usize),
            _ => None
        }
    }
}
