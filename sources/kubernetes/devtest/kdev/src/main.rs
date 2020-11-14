use clap::Clap;

#[derive(Clap)]
#[clap(version = "1.0", author = "Daniel Frederico Lins Leite <1985.daniel@gmail>")]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    #[clap(version = "1.3", author = "Daniel Frederico Lins Leite <1985.daniel@gmail>")]
    Cluster(Cluster),
}

/// Manage this machine clusters
#[derive(Clap)]
struct Cluster {
    #[clap(subcommand)]
    subcmd: ClusterSubCommand,
}

#[derive(Clap)]
enum ClusterSubCommand {
    #[clap(version = "1.3", author = "Daniel Frederico Lins Leite <1985.daniel@gmail>")]
    Status(ClusterStatus),
}

/// Query Cluster Status
#[derive(Clap)]
struct ClusterStatus {
}

fn find_exe(name: &str) -> Result<String,String>
{
    let output = std::process::Command::new("which")
        .arg(name)
        .output()
        .map_err(|_| "Cannot find 'kind'")?;
    let stdout = std::str::from_utf8(&output.stdout)
        .map_err(|_| "Cannot find 'kind'")?;
    Ok(stdout.trim_end_matches("\r").trim_end_matches("\n").to_string())
}

fn run_exe(program: &str, args: &[&str]) -> Result<(String,String),String>
{
    let output = std::process::Command::new(program)
        .args(args)
        .output()
        .map_err(|_| "ERROR")?;
    let stdout = std::str::from_utf8(&output.stdout)
        .map_err(|_| "ERROR")?;
    let stderr = std::str::from_utf8(&output.stderr)
        .map_err(|_| "ERROR")?;
    Ok((stdout.to_string(),stderr.to_string()))
}

fn run_cluster_status(opts: &Opts, c: &Cluster, s: &ClusterStatus) -> std::result::Result<String,String> 
{
    let kind = find_exe("kind")?;
    let (clusters,_) = run_exe(&kind, &["get", "clusters"])?;
    let cluster_name = clusters.split("\n").take(1).nth(0).unwrap();

    let kubectl = find_exe("kubectl")?;
    let (pv, _) = run_exe(&kubectl, &["get", "pv", "source-code-volume", "-o", "JSON"])?;

    use json::*;
    let json = json::parse(pv.as_str()).unwrap();
    println!("pv: {:?}", json["spec"]);

    Ok(format!("Kind cluster detected: [{}]", cluster_name))
}

fn main() {
    let opts: Opts = Opts::parse();
    let r = match &opts.subcmd {
        SubCommand::Cluster(c) => {
            match &c.subcmd {
                ClusterSubCommand::Status(s) => {
                    run_cluster_status(&opts, &c, &s)
                }
            }
        }
    };
    match r {
        Ok(r) =>  println!("{}", r),
        Err(e) => eprint!("{}", e)
    }
}
