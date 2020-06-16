#[macro_use]
extern crate cdrs;

mod actors;
mod app;

use actors::cql::start_cql;
use actors::kafka::start_kafka_consumer;
use actors::websocket::start_ws_server;
use app::ws_handler::start_ws_handler;
use log::debug;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(name = "basic")]
struct Opt {
    #[structopt(short = "b")]
    brokers: Vec<String>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    let opt = Opt::from_args();
    debug!("{:?}", opt);

    let hostname = ::hostname::get()?.to_str().ok_or("no hostname")?.to_owned();
    let topic = hostname.clone();
    let brokers = opt.brokers.join(",");
    debug!("{:?} {:?}", topic, brokers);

    let cluster_address = "127.0.0.1:9042".to_owned();
    let cql = start_cql(cluster_address);

    let key = b"12345678a";
    let wscommands = start_ws_handler(key, hostname, cql);

    let bind_addr = "0.0.0.0:9002".to_string();
    let _ = start_ws_server(bind_addr, wscommands).await;

    let rx = start_kafka_consumer(brokers, topic);
    loop {
        let msg = rx.recv().unwrap();
        debug!("{:?}", msg);
    }
}
