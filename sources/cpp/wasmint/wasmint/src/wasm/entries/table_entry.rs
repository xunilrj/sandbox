use crate::utils::*;
use crate::wasm::*;

#[derive(Debug)]
pub struct TableEntry
{
    elem_type: DataTypes,
    initial: u32,
    maximum: Option<u32>,
}
impl TableEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let elem_type = DataTypes::from(f)?;

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

        Ok(TableEntry{ elem_type, initial, maximum })
    }
}
