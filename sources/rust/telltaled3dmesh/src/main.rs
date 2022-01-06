
mod anm;
mod d3dmesh;
mod emulator;
mod parser;
mod skl;
mod gltf;

use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(about = "D3DMesh converter")]
pub struct MeshConvertArgs {
    #[structopt(short, long)]
    path: String,
    /// Relative or absolute path of the file that will be generated. Supported formats: .json, .obj.
    #[structopt(short, long)]
    output: Option<String>,
    #[structopt(long)]
    pretty_print: bool,
    #[structopt(long)]
    buffer_as_base64: bool,
    #[structopt(long)]
    detach_index_buffer: bool,
}

#[derive(StructOpt, Debug)]
#[structopt(about = "SKL converter")]
pub struct SklConvertArgs {
    #[structopt(short, long)]
    path: String,
}

#[derive(StructOpt, Debug)]
#[structopt(about = "Anm converter")]
pub struct AnmConvertArgs {
    #[structopt(short, long)]
    path: String,
}

#[derive(StructOpt, Debug)]
#[structopt(about = "Telltale Games converters")]
enum Args {
    MeshConvert(MeshConvertArgs),
    SklConvert(SklConvertArgs),
    AnmConvert(AnmConvertArgs),
}

fn main() {
    pretty_env_logger::init();

    let args = Args::from_args();
    //println!("{:?}", args);

    match args {
        Args::MeshConvert(MeshConvertArgs {
            path,
            output,
            pretty_print,
            buffer_as_base64,
            detach_index_buffer,
        }) => {
            d3dmesh::convert(
                path,
                output,
                pretty_print,
                buffer_as_base64,
                detach_index_buffer,
            )
            .unwrap();
        }
        Args::SklConvert(SklConvertArgs { path }) => skl::convert(path),
        Args::AnmConvert(AnmConvertArgs { path }) => anm::convert(path),
    }
}
