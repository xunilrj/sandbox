CREATE KEYSPACE chat WITH REPLICATION = {'class':'SimpleStrategy', 'replication_factor':1};

CREATE TABLE users (
    id uuid,
    name text,
    online bool,
    image blob,
    PRIMARY KEY (id)
)

CREATE TABLE users_ws (
    name text,
    hostname text,
    PRIMARY KEY (name)
);

CREATE TABLE threads (
    id text,
    last_timeslice int,
    PRIMARY KEY (id)
)

CREATE TABLE messages (
    thread_id text,
    timeslice int,
    createdon timestamp,   
    user_id uuid,
    user_name uuid,
    message text,
    versions set<timeuuid>,
    reactions map<uuid, int>,
    attached blob,
    PRIMARY KEY ((thread_id,timeslice), createdon)
)

CREATE TABLE messages_version (       
    id timeuuid,
    createdon timestamp,   
    json text,    
    PRIMARY KEY (createdon)
)