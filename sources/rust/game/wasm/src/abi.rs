use std::{fmt::Debug, result::Result};
use wasm_bindgen::{
    convert::FromWasmAbi,
    describe::{inform, WasmDescribe},
    JsValue,
};

pub type JsResult<T> = Result<T, JsValue>;

pub trait Context<T> {
    fn context(self, msg: &dyn Debug) -> JsResult<T>;
}

impl<T, TError> Context<T> for Result<T, TError>
where
    TError: Debug,
{
    fn context(self, msg: &dyn Debug) -> JsResult<T> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(format!("{:?} {:?}", msg, err).into()),
        }
    }
}

pub struct LogLevel(pub log::Level);

impl WasmDescribe for LogLevel {
    fn describe() {
        inform(wasm_bindgen::describe::I8);
    }
}

impl FromWasmAbi for LogLevel {
    type Abi = u32;
    unsafe fn from_abi(js: Self::Abi) -> Self {
        match js {
            0 => LogLevel(log::Level::Trace),
            1 => LogLevel(log::Level::Debug),
            2 => LogLevel(log::Level::Info),
            3 => LogLevel(log::Level::Warn),
            _ => LogLevel(log::Level::Error),
        }
    }
}
