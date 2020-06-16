use crate::actors::cql::CqlCommand;
use crate::actors::websocket::{LoginCommand, WsCommands};
use jwt::{Claims, Header, Token};
use log::{debug, error};
use std::result::Result;

use thiserror::Error;
use tokio::sync::mpsc::channel;
use tokio::sync::mpsc::Receiver;
use tokio::sync::mpsc::Sender;

#[derive(Error, Debug)]
enum HandleError {
    #[error("Syntax error: [<COMMAND>:{{...}}]")]
    InvalidJWTKey,
    #[error("Syntax error: [<COMMAND>:{{...}}]")]
    InvalidJWTToken,
    #[error("Syntax error: [<COMMAND>:{{...}}]")]
    MussingSubClaim,
    #[error("Syntax error: [<COMMAND>:{{...}}]")]
    ChannelDied,
}

async fn handle(
    msg: WsCommands,
    key: &[u8],
    hostname: &str,
    mut cql: Sender<CqlCommand>,
) -> Result<(), HandleError> {
    match msg {
        WsCommands::Login(LoginCommand { token }) => {
            let t: Token<Header, Claims, _> = jwt::Token::parse_unverified(token.as_str())
                .or(Err(HandleError::InvalidJWTToken))?;
            let sub = t.claims().registered.subject.as_ref();
            let sub = sub.ok_or(HandleError::MussingSubClaim)?;
            debug!("{}", sub);
            let q =
                "INSERT INTO chat.users_ws (name, hostname) VALUES (:name,:hostname) USING TTL 10;"
                    .to_string();
            let values = query_values!("name" => sub.as_str(), "hostname" => hostname);
            let _ = cql.send(CqlCommand::Run(q, values)).await;
        }
    }
    Ok(())
}

async fn ws_handler(
    mut receiver: Receiver<WsCommands>,
    key: Vec<u8>,
    hostname: String,
    cql: Sender<CqlCommand>,
) -> Result<(), HandleError> {
    async {}.await;
    let cql = cql.clone();
    loop {
        let r = receiver.recv().await;
        if r.is_none() {
            return Err(HandleError::ChannelDied);
        }

        let msg = r.unwrap();
        debug!("ws_handler: {:?}", &msg);
        match handle(msg, key.as_slice(), &hostname, cql.clone()).await {
            Err(e) => error!("{:?}", e),
            _ => {}
        }
    }
    // Ok(())
}

pub fn start_ws_handler(
    key: &[u8],
    hostname: String,
    cql: Sender<CqlCommand>,
) -> Sender<WsCommands> {
    let key = key.to_owned();
    let (sender, receiver) = channel::<WsCommands>(100);
    let cql = cql.clone();
    tokio::spawn(ws_handler(receiver, key, hostname, cql));
    sender
}
