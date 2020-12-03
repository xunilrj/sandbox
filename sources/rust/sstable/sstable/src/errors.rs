use log::error;
use std::fmt::Debug;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum FlushError {
    #[error("Error")]
    Error(String),
}

impl std::convert::From<std::io::Error> for FlushError {
    fn from(e: std::io::Error) -> Self {
        FlushError::Error(format!("{}", e))
    }
}
