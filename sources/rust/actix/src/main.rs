use actix_web::Error;
use actix::prelude::*;
use actix_web::{get, web, App, HttpServer, Responder};
use listenfd::ListenFd;

mod db;
use db::{new_db, create_db, DbActor,  StartMigration};

#[get("/")]
async fn index(db: web::Data<Addr<DbActor>>) -> Result<impl Responder, Error>
{
    // let r = db.send(CreateUser{name: "daniel".to_owned()}).await?;
    // println!("Result: {}", r.expect(""));
    Ok("Hello")
}

#[actix_rt::main]
async fn main() -> std::io::Result<()>
{
    pretty_env_logger::init();

    //let database_url = env::var("DATABASE_URL")
        //.expect("DATABASE_URL must be set");
    
    let db_url = "postgres://postgres:12345678a@localhost/actix";
    create_db(db_url);
    let db = SyncArbiter::start(3, move || new_db(db_url).expect("connection"));
    let t = db.send(StartMigration{}).await;

    let mut server = HttpServer::new(move || 
        App::new()
            .data(db.clone())
            .service(index)
        );

    let mut listenfd = ListenFd::from_env();
    server = if let Some(l) = listenfd.take_tcp_listener(0).unwrap() {
        server.listen(l)?
    } else {
        server.bind("127.0.0.1:3000")?
    };

    server.run().await
}