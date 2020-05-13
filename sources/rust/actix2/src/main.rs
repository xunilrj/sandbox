use std::ops::Deref;
use actix_web::{get, web, App, HttpServer, Responder};
use std::{sync::Mutex, io::Result};
use serde_derive::*;

#[derive(Clone)]
struct AppState
{
    count: u32,
}

impl AppState
{
    fn inc(&mut self)
    {
        self.count += 1;
    }
}

#[get("/{id}/{name}/index.html")]
async fn index(data: web::Data<Mutex<AppState>>) -> Result<impl Responder>
{    
    let mut v = data.lock()?.await;
    v.inc();
    format!("Hello {}", v.deref().count)
}

#[actix_rt::main]
async fn main() -> Result<()>
{
    let state = web::Data::new(Mutex::new(AppState{ count: 0 }));

    HttpServer::new(move ||
            App::new()
            .app_data(state.clone())
            .service(index)
        )
        .bind("127.0.0.1:8080")?
        .run()
        .await
}