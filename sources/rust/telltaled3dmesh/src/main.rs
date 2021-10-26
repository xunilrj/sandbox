#![feature(with_options)]

mod d3dfile;
mod indexbuffer;
pub mod outputs;
pub mod run;

use d3dfile::parse_d3dfile;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(about = "Telltale Games D3DMesh converter")]
enum Args {
    Convert {
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
    },
}

fn main() {
    pretty_env_logger::init();

    let args = Args::from_args();
    //println!("{:?}", args);

    match args {
        Args::Convert {
            path,
            output,
            pretty_print,
            buffer_as_base64,
            detach_index_buffer,
        } => {
            parse_d3dfile(
                path,
                output,
                pretty_print,
                buffer_as_base64,
                detach_index_buffer,
            )
            .unwrap();
        }
    }
}
