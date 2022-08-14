use std::{io::Cursor, path::PathBuf};

use log::trace;
use structopt::StructOpt;
use user_error::UserFacingError;

use crate::{
    checksum_mapping::ChecksumMap,
    formats::{anm::AnmParser, d3dmesh::D3dMeshParser, skl::SklParser},
    utils::create_dir_all,
    Progress,
};

#[derive(StructOpt, Debug)]
pub struct ConvertAnmArgs {
    /// Path to the anm file
    #[structopt(short, long)]
    anm: String,

    /// Path to the skl file
    #[structopt(long)]
    mesh: String,

    /// Path to the skl file
    #[structopt(long)]
    skl: String,

    /// Formats: json = data dump, gltf = nodes and skins
    #[structopt(short, long)]
    output: String,
}

impl ConvertAnmArgs {
    pub fn run(
        &self,
        mapping: &ChecksumMap,
        progress: &mut Progress,
    ) -> Result<(), UserFacingError> {
        trace!("Parsing mesh: {:?}", self.mesh);
        let mut parser = D3dMeshParser::new();
        let _ = parser.parse(&self.mesh, mapping).unwrap();

        trace!("Parsing skl: {:?}", self.skl);
        let mut parser = SklParser::new();
        let mut skl = parser.parse(&self.skl, mapping).unwrap();
        skl.calculate_inverse_bind_pose();

        let mut parser = AnmParser::new();
        let anm = parser.parse(&self.anm, mapping).unwrap();

        trace!("Saving anm to: {:?}", self.output);
        let output: PathBuf = self.output.clone().into();
        match output.extension().and_then(|ext| ext.to_str()) {
            Some("gltf") => {
                progress.update_text("Converting to GLTF");
                let mut gltf = crate::formats::gltf::empty();
                crate::formats::skl::gltf::to_gltf(&skl, &mut gltf);
                crate::formats::anm::gltf::to_gltf(&skl, &anm, &mut gltf);

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
