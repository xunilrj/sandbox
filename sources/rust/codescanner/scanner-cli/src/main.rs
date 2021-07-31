mod console_reporter;

use scan::reporter::ScanReporter;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "basic")]
enum Commands {
    Scan {
        /// Path to the root of th source code
        #[structopt(short, long)]
        path: String,

        /// Verbose output
        #[structopt(short, long)]
        verbose: bool,
    },
}

fn main() {
    let cmds = Commands::from_args();
    match &cmds {
        Commands::Scan { path, verbose } => {
            if *verbose {
                println!("{:#?}", cmds);
            }
            let ctx = scanner::scan(path.as_str());

            let mut console = console_reporter::ConsoleScanReporter::new();
            let _ = console.report(&ctx.map(|x| x.result));
        }
    }
}
