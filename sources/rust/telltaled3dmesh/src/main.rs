mod anm;
mod d3dmesh;
mod emulator;
mod gltf;
mod parser;
mod skl;

use parser::parse_le_u32;
use structopt::StructOpt;

use crate::parser::whats_next;

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
    #[structopt(long)]
    texture_path: Option<String>,
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
    /// Relative or absolute path of the file that will be generated. Supported formats: .gltf.
    #[structopt(short, long)]
    output: String,
}

#[derive(StructOpt, Debug)]
#[structopt(about = "D3DTX converter")]
pub struct D3dtxConvertArgs {
    #[structopt(short, long)]
    path: String,
}

#[derive(StructOpt, Debug)]
#[structopt(about = "Telltale Games converters")]
enum Args {
    MeshConvert(MeshConvertArgs),
    SklConvert(SklConvertArgs),
    AnmConvert(AnmConvertArgs),
    D3dtxConvert(D3dtxConvertArgs),
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
            texture_path,
        }) => {
            d3dmesh::convert(
                path,
                output,
                pretty_print,
                buffer_as_base64,
                detach_index_buffer,
                texture_path,
            )
            .unwrap();
        }
        Args::SklConvert(SklConvertArgs { path }) => skl::convert(path),
        Args::AnmConvert(AnmConvertArgs { path, output }) => anm::convert(path, output),
        Args::D3dtxConvert(D3dtxConvertArgs { path }) => {
            let input = std::fs::read(path).unwrap();
            let mut input = parser::NomSlice::new(input.as_slice());

            if !input.read_ertm_magic_number() {
                panic!("ERTM not found");
            }

            let properties = input.read_properties();
            dbg!(properties);

            //t3texture : 3E249E7E6F38CCE2
            //flags : 84283CB979D71641

            let d3d_name = input.parse_length_string("?");
            let _ = input.parse_le_u32("?");

            let _ = input.parse_n_bytes(6);

            let _ = input.parse_le_u32("?");
            // https://en.wikipedia.org/wiki/S3_Texture_Compression
            let codec = input.parse_string(4, "codec");

            let _ = input.parse_n_bytes(58);

            // https://wiki.beyondskyrim.org/wiki/Arcane_University:DDS_Data_Format
            let format = input.parse_string(4, "format");

            whats_next(input.slice);

            // let v = input.parse_le_i32("version");
            // let block_size = input.parse_le_i32("sampler block size");
            // let sampler_state = input.parse_le_u32("sampler state");
            // let platform_block_size = input.parse_le_u32("platform block size");
            // let platform = input.parse_le_u32("platform");
        }
    }
}
