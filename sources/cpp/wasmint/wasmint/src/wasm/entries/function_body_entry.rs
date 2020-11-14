use std::fmt::Debug;
use crate::utils::*;
use crate::wasm::*;
use crate::entries::local_entry::*;

#[derive(Debug)]
pub struct FunctionBodyEntry
{
    body_size: u32,
    locals: Vec<LocalEntry>,
    body: FunctionBody
}

impl FunctionBodyEntry
{
    pub fn from<T: std::io::Read>(f: &mut T) -> Result<Self, WasmError>
    {
        let (body_size,_) = read_uleb128(f)
            .map_err(WasmError::IOError)?;
        let locals = vec_uleb128(f, LocalEntry::from)
            .map_err(WasmError::IOError)?;
        let body = FunctionBody::from(f)?;

        Ok(FunctionBodyEntry{ body_size, locals, body })
    }

    pub fn get_instruction(&self, index: usize) -> Option<&OpCode>
    {
        self.body.instructions.get(index)
    }
}