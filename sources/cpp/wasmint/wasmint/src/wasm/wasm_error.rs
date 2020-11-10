#[derive(Debug)]
pub enum WasmError
{
    IOError(std::io::Error),
    UnkownKind,
    InvalidOpcode,
    InvalidThread,
    NotImplemented
}

impl std::convert::From<WasmError> for std::io::Error
{
    fn from(e: WasmError) -> Self {
        match e {
            WasmError::IOError(e) => e,
            err @ _ => std::io::Error::new(std::io::ErrorKind::Other, format!("{:?}", err))
        }
    }
}