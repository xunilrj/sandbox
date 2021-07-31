use scan::{reporter::ScanReporter, ScanError, ScanResult};

pub struct ConsoleScanReporter {}

impl ConsoleScanReporter {
    pub fn new() -> Self {
        Self {}
    }
}

impl ScanReporter for ConsoleScanReporter {
    fn report(
        &mut self,
        result: &std::result::Result<ScanResult, ScanError>,
    ) -> std::result::Result<(), scan::reporter::ScanReportError> {
        use ansi_rgb::{red, Foreground};
        match result {
            Ok(result) => {
                for finding in result.findings.iter() {
                    println!(
                        "{}: \"{}\" at {}",
                        format!("{:?}", finding.t).fg(red()),
                        finding.description,
                        finding.fmt_at()
                    );
                }
            }
            Err(_) => todo!(),
        }

        Ok(())
    }
}
