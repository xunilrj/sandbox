use auto_from::From;

use crate::{ScanError, ScanResult};

#[derive(Debug, From)]
pub enum ScanReportError {
    UnkownError,
    IO(std::io::Error),
}

pub trait ScanReporter {
    fn report(
        &mut self,
        result: &std::result::Result<ScanResult, ScanError>,
    ) -> std::result::Result<(), ScanReportError>;
}
