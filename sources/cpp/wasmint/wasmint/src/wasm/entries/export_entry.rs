use crate::utils::*;
use crate::wasm::*;

#[derive(Debug)]
pub struct ExportEntry
{
    pub field: String,
    pub kind: ExternalKind,
}
impl ExportEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let (field,_) = read_uleb128_string(f)
            .map_err(WasmError::IOError)?;
        let kind = ExternalKind::from(f)?;

        Ok(ExportEntry{ field, kind })
    }
}

