use diesel::result::Error::DatabaseError;
use diesel::ConnectionError::BadConnection;
use actix::prelude::*;
use actix::{Actor, Message, Handler};
use actix_derive::{MessageResponse};
use diesel::prelude::*;
use diesel::pg::PgConnection;
use url::{Url, ParseError};
use log::{info, trace, warn, error};

mod embedded
{
    use refinery::embed_migrations;
    embed_migrations!("src/db/migrations");
}

#[derive(Message)]
#[rtype(result="Result<usize, bool>")]
pub struct StartMigration { }

pub struct CreateUser 
{
    pub name: String,
}

impl Message for CreateUser
{
    type Result = Result<String, String>;
}

pub struct DbActor
{
    pub conn: PgConnection,
}

pub fn create_db(url: &str)
{
    let db_url = Url::parse(url).expect("");
    
    ////Server=localhost;Port=5432;User Id=postgres;Password=enter;
    let server_url = format!("{}://{}:{}@{}",
        db_url.scheme(),
        db_url.username(),
        db_url.password().unwrap(),
        db_url.host_str().unwrap()
    );
    trace!("create_db: {}", server_url);

    let conn = PgConnection::establish(&server_url).expect("");
    match conn.execute("CREATE DATABASE actix")
    {
        Err(DatabaseError(diesel::result::DatabaseErrorKind::__Unknown, info)) 
            if info.as_ref().message() == "database \"actix\" already exists"
            => { trace!("{}", info.as_ref().message()); Ok(1) }
        x => x
    }.expect("Unexpected Error");
}

pub fn new_db (url: &str) -> Result<DbActor, ConnectionError>
{
    trace!("new_db: {}", url);
    let c = PgConnection::establish(&url)?;
    Ok(DbActor { conn: c })
}

impl Actor for DbActor
{
    type Context = SyncContext<Self>;
}

impl Handler<StartMigration> for DbActor
{
    type Result = Result<usize, bool>;

    fn handle(&mut self, msg: StartMigration, _: &mut Self::Context) -> Self::Result
    {
        println!("Start migration");
        // embedded::migrations::runner()
        //     .run(&mut self.conn)
        //     .unwrap();
        let r = 1;
        Ok(r)
    }
}


impl Handler<CreateUser> for DbActor
{
    type Result = Result<String, String>;

    fn handle(&mut self, msg: CreateUser, _: &mut Self::Context) -> Self::Result
    {
        let r = format!("Hello {} 4", msg.name);
        Ok(r)
    }
}