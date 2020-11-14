use std::fmt::Debug;
use crate::utils::*;
use crate::wasm::*;

#[derive(Debug)]
pub struct GlobalEntry
{
    content_type: DataTypes,
    mutable: bool,
    body: FunctionBody,
}
impl GlobalEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let content_type = DataTypes::from(f)?;
        let mutable = read_uleb128(f)
            .map_err(WasmError::IOError)?.0 == 1;

        let body = FunctionBody::from(f)?;

        Ok(GlobalEntry{ content_type, mutable, body })
    }
}
