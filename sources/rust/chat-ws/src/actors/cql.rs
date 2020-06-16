use cdrs::authenticators::NoneAuthenticator;
use cdrs::cluster::session::new as new_session;
use cdrs::cluster::{ClusterTcpConfig, NodeTcpConfigBuilder};
use cdrs::load_balancing::RoundRobin;
use cdrs::query::QueryValues;
use cdrs::query::*;
use log::{debug, error};
use tokio::sync::mpsc::{channel, Sender};

#[derive(Debug)]
pub enum CqlCommand {
    Run(String, QueryValues),
}

pub fn start_cql(cluster_address: String) -> Sender<CqlCommand> {
    let (tx, mut rx) = channel::<CqlCommand>(100);

    let _ = tokio::spawn(async move {
        debug!("cql: start");
        let node = NodeTcpConfigBuilder::new(&cluster_address, NoneAuthenticator {}).build();
        let cluster_config = ClusterTcpConfig(vec![node]);
        let session =
            new_session(&cluster_config, RoundRobin::new()).expect("session should be created");

        loop {
            let r = rx.recv().await;
            if r.is_none() {
                error!("cql: {:?}", r);
                break;
            }
            let msg = r.unwrap();
            match msg {
                CqlCommand::Run(q, values) => {
                    debug!("{} {:?}", q, values);
                    session
                        .query_with_values(&q, values)
                        .expect("Keyspace create error");
                }
            }
        }
        debug!("cql: end");
    });
    tx
}
