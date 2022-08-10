mod checksum_mapping;
mod commands;
mod formats;
mod parser;
mod utils;

use checksum_mapping::ChecksumMap;
use commands::{
    convert_anm::ConvertAnmArgs, convert_chore::ConvertChoreArgs, convert_skl::ConvertSklArgs,
};
use log::debug;
use spinoff::{Color, Spinner, Spinners};
use structopt::StructOpt;
use user_error::UFE;

#[derive(StructOpt, Debug)]
#[structopt(about = "Tales of Monkey Island converter")]
enum Args {
    /// Convert SKL files
    ConvertSkl(ConvertSklArgs),
    /// Convert ANM files
    ConvertAnm(ConvertAnmArgs),
    /// Convert CHORE files
    ConvertChore(ConvertChoreArgs),
}

pub enum Progress {
    Spinner(Spinner),
    Nothing,
}

impl Progress {
    pub fn success(self, msg: &str) {
        match self {
            Progress::Spinner(spinner) => {
                spinner.success(msg);
            }
            Progress::Nothing => {}
        }
    }

    pub fn fail(self, msg: &str) {
        match self {
            Progress::Spinner(spinner) => {
                spinner.fail(msg);
            }
            Progress::Nothing => {}
        }
    }

    pub fn update_text(&mut self, msg: &'static str) {
        match self {
            Progress::Spinner(spinner) => {
                spinner.update_text(msg);
            }
            Progress::Nothing => {}
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let args = Args::from_args();
    debug!("{:?}", args);

    let mapping = ChecksumMap::new();

    let mut progress = match std::env::var("RUST_LOG") {
        Ok(_) => Progress::Nothing,
        Err(_) => Progress::Spinner(Spinner::new(Spinners::Dots, "Exporting...", Color::Blue)),
    };

    let r = match args {
        Args::ConvertSkl(args) => args.run(&mapping, &mut progress),
        Args::ConvertAnm(args) => args.run(&mapping, &mut progress),
        Args::ConvertChore(args) => args.run(&mapping, &mut progress),
    };

    match r {
        Ok(_) => {
            progress.success("Done!");
        }
        Err(err) => {
            progress.fail("Fail");
            err.print_and_exit();
        }
    };
}
