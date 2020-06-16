use anyhow::Result;
use futures_util::{SinkExt, StreamExt};
use hmac::Mac;
use log::trace;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::mpsc::Sender;
use tokio_tungstenite::tungstenite::protocol::Message;
use tokio_tungstenite::tungstenite::Message::Text;

#[derive(Serialize, Deserialize, Debug)]
pub struct LoginCommand {
    pub token: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum WsCommands {
    Login(LoginCommand),
}

#[derive(Error, Debug)]
enum ParseError {
    #[error("Syntax error: [<COMMAND>:{{...}}]")]
    SyntaxError,
    #[error("Invalid Command")]
    InvalidCommand,
    #[error("Invalid Json")]
    InvalidJson,
}

impl WsCommands {
    fn from_json(name: &str, json: &str) -> Result<WsCommands, ParseError> {
        match name {
            "login" => {
                let v: LoginCommand =
                    serde_json::from_str(json).or(Err(ParseError::InvalidJson))?;
                Ok(WsCommands::Login(v))
            }
            _ => Err(ParseError::InvalidCommand),
        }
    }
}

fn parse_cmd(msg: Message) -> Result<Option<WsCommands>, ParseError> {
    match msg {
        Text(txt) => match txt.find(":") {
            Some(pos) => {
                let json = txt[pos + 1..].trim();
                let cmd = WsCommands::from_json(&txt[0..pos], json)?;
                Ok(Some(cmd))
            }
            None => Err(ParseError::SyntaxError),
        },
        _ => Ok(None),
    }
}

async fn accept_connection(
    stream: TcpStream,
    mut wscommands: Sender<WsCommands>,
) -> anyhow::Result<()> {
    let addr = stream.peer_addr()?;
    trace!("Websocket: Accepting: {}", addr);
    let mut ws_stream = tokio_tungstenite::accept_async(stream).await?;

    loop {
        let r = ws_stream.next().await;
        if r.is_none() {
            return Ok(());
        }
        let msg = r.unwrap()?;
        match parse_cmd(msg) {
            Ok(Some(cmd)) => {
                wscommands.send(cmd).await?;
            }
            Ok(None) => {}
            Err(e) => {
                let msg = Message::Text(e.to_string());
                ws_stream.send(msg).await?;
            }
        }
    }
}

async fn listen(bind_addr: String, wscommands: Sender<WsCommands>) -> anyhow::Result<()> {
    trace!("Websocket: {}...", bind_addr);
    let mut server = TcpListener::bind(bind_addr).await?;
    loop {
        let (stream, _) = server.accept().await?;
        trace!("WebSocket: New Connection: {:?}", stream);
        let wscommands = wscommands.clone();
        tokio::spawn(accept_connection(stream, wscommands));
    }
}

pub fn start_ws_server(
    bind_addr: String,
    wscommands: Sender<WsCommands>,
) -> tokio::task::JoinHandle<anyhow::Result<()>> {
    tokio::spawn(listen(bind_addr, wscommands))
}
