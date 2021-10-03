#![allow(dead_code)]
#![feature(with_options)]
#![feature(asm)]

mod indexbuffer;
pub mod run;
mod d3dmesh;

use d3dmesh::parse_d3dmesh;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(about = "Telltale Games D3DMesh converter")]
enum Args {
    Convert {
        #[structopt(short, long)]
        path: String,
        #[structopt(short, long)]
        output: Option<String>,
        #[structopt(long)]
        pretty_print: bool,
        #[structopt(long)]
        buffer_as_base64: bool,
    },
}

fn main() {
    let args = Args::from_args();
    //println!("{:?}", args);

    match args {
        Args::Convert {
            path,
            output,
            pretty_print,
            buffer_as_base64,
        } => {
            parse_d3dmesh(path, output, pretty_print, buffer_as_base64).unwrap();
        }
    }
}
