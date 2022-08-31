use std::{io::Cursor, path::PathBuf};

use log::trace;
use structopt::StructOpt;
use user_error::UserFacingError;

use crate::{
    checksum_mapping::ChecksumMap, formats::skl::SklParser, utils::create_dir_all, Progress,
};

#[derive(StructOpt, Debug)]
pub struct ConvertSklArgs {
    /// Path to the skl file
    #[structopt(short, long)]
    path: String,

    /// Formats: json = data dump, gltf = nodes and skins
    #[structopt(short, long)]
    output: String,
}

impl ConvertSklArgs {
    pub fn run(
        &self,
        mapping: &ChecksumMap,
        progress: &mut Progress,
    ) -> Result<(), UserFacingError> {
        let mut parser = SklParser::new();

        trace!("Parsing skl: {:?}", self.path);
        progress.update_text("Parsing SKL");
        let mut skl = parser.parse(&self.path, mapping).map_err(|e| {
            UserFacingError::new("Failed when parsing file")
                .reason(e.user_facing_reason())
                .help("Set the variable RUST_LOG=debug for more details")
        })?;
        skl.calculate_inverse_bind_pose();

        trace!("Saving skl to: {:?}", self.output);
        let output: PathBuf = self.output.clone().into();
        match output.extension().and_then(|ext| ext.to_str()) {
            // Some("json") => {
            //     progress.update_text("Saving JSON");
            //     let _ = skl.to_json(&output).map_err(|_| {
            //         UserFacingError::new("Failed saving JSON file")
            //             .help("Set the variable RUST_LOG=debug for more details")
            //     })?;
            //     Ok(())
            // }
            Some("gltf") => {
                progress.update_text("Converting to GLTF");
                let mut gltf = crate::formats::gltf::empty();
                crate::formats::skl::gltf::to_gltf(&skl, &mut gltf);

                progress.update_text("Saving GLTF file");
                let parent = output.parent().ok_or_else(|| {
                    UserFacingError::new("Failed saving file")
                        .help("Set the variable RUST_LOG=debug for more details")
                })?;
                let _ = create_dir_all(parent).map_err(|_| {
                    UserFacingError::new("Failed saving file")
                        .help("Set the variable RUST_LOG=debug for more details")
                })?;

                let buffer = vec![];
                let mut json = Cursor::new(buffer);
                let _ = gltf.write_pretty(&mut json, 4);

                let json = String::from_utf8(json.into_inner()).unwrap();
                let _ = std::fs::write(&output, json);

                Ok(())
            }
            _ => Err(UserFacingError::new("Invalid output file")
                .help("Run the application with -h for more details")),
        }
    }
}
