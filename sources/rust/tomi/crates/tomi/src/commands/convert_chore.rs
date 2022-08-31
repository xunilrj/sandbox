use log::trace;
use structopt::StructOpt;
use user_error::UserFacingError;

use crate::{checksum_mapping::ChecksumMap, formats::chore::ChoreParser, Progress};

#[derive(StructOpt, Debug)]
pub struct ConvertChoreArgs {
    /// Path to the chore file
    #[structopt(short, long)]
    path: String,
    // Formats: gltf = nodes and skins
    // #[structopt(short, long)]
    // output: String,
}

impl ConvertChoreArgs {
    pub fn run(&self, mapping: &ChecksumMap, _: &mut Progress) -> Result<(), UserFacingError> {
        trace!("Parsing chore: {:?}", self.path);
        let mut parser = ChoreParser::new();
        let _ = parser.parse(&self.path, mapping).unwrap();

        todo!();

        // trace!("Saving chore to: {:?}", self.output);
        // let output: PathBuf = self.output.clone().into();
        // match output.extension().and_then(|ext| ext.to_str()) {
        //     Some("gltf") => {
        //         progress.update_text("Converting to GLTF");
        //         let mut gltf = crate::formats::gltf::empty();
        //         crate::formats::skl::gltf::to_gltf(&skl, &mut gltf);
        //         crate::formats::anm::gltf::to_gltf(&skl, &anm, &mut gltf);

        //         progress.update_text("Saving GLTF file");
        //         let parent = output.parent().ok_or_else(|| {
        //             UserFacingError::new("Failed saving file")
        //                 .help("Set the variable RUST_LOG=debug for more details")
        //         })?;
        //         let _ = create_dir_all(parent).map_err(|_| {
        //             UserFacingError::new("Failed saving file")
        //                 .help("Set the variable RUST_LOG=debug for more details")
        //         })?;

        //         let buffer = vec![];
        //         let mut json = Cursor::new(buffer);
        //         let _ = gltf.write_pretty(&mut json, 4);

        //         let json = String::from_utf8(json.into_inner()).unwrap();
        //         let _ = std::fs::write(&output, json);

        //         Ok(())
        //     }
        //     _ => Err(UserFacingError::new("Invalid output file")
        //         .help("Run the application with -h for more details")),
        // }
    }
}
