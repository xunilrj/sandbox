use log::{debug, info};
use std::{
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct IOError {
    pub path: PathBuf,
    pub error: std::io::Error,
}

pub trait DebugError {
    type Error;
    fn debug_err(self, path: &Path) -> Self::Error;
}

impl<T> DebugError for Result<T, std::io::Error> {
    type Error = Result<T, IOError>;
    fn debug_err(self, path: &Path) -> Self::Error {
        match self {
            Ok(v) => Ok(v),
            Err(error) => {
                debug!("{:?}", error);
                Err(IOError {
                    path: path.to_path_buf(),
                    error,
                })
            }
        }
    }
}

pub fn create_dir_all(path: impl AsRef<Path>) -> Result<(), IOError> {
    let path = path.as_ref();
    debug!("Creating dir: {:?}", &path);
    std::fs::create_dir_all(&path).debug_err(path)
}

pub fn read_to_end(path: impl AsRef<Path>) -> Result<Vec<u8>, IOError> {
    let path = path.as_ref();

    debug!("Reading file to memory: {:?}", &path);
    let mut f = File::open(path).debug_err(path)?;
    let mut bytes = vec![];
    let _ = f.read_to_end(&mut bytes).debug_err(path)?;
    info!("File {:?} has {} bytes", &path, bytes.len());
    Ok(bytes)
}
