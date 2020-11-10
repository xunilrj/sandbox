use crate::wasm::*;

#[derive(Debug)]
pub struct LocalEntry
{
    t: DataTypes
}

impl LocalEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        Ok(LocalEntry{ t: DataTypes::from(f)? })
    }
}
