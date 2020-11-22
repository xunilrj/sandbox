#![feature(with_options)]
#![feature(test)]

use streams_rs::*;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = std::env::args().collect();
    println!("{:?}", args);

    let mut x = Streams::new(".");

    if let Some(arg1) = args.get(1) {
        if arg1 == "consumer"
        {
            let stream = args.get(2).unwrap();
            let group = args.get(3).unwrap();
            let consumer = args.get(4).unwrap();
            
            loop
            {
                match x.group_read(&group, &consumer, &stream, true) {
                    Ok(msgs) => {
                        for msg in msgs {
                            let ack = x.ack(stream, group, &StreamId::Specific(msg.timestamp, msg.order));
                            println!("{:?} {:?}", msg, ack);
                        }
                    },
                    Err(e) => return Err(e)
                }
            }
        }
        else if arg1 == "producer"
        {
            let stream = args.get(2).unwrap();

            use std::time::{SystemTime, UNIX_EPOCH};
            let start = SystemTime::now();                

            for i in 0..10
            {
                let id = x.add(&stream, StreamId::Asterisk).unwrap();
            }

            let end = SystemTime::now();          
            println!("Took: {:?}", end.duration_since(start));
        }
        else if arg1 == "pending" 
        {
            let stream = args.get(2).unwrap();
            let group = args.get(3).unwrap();

            let r = x.pending(stream, group).unwrap();
            println!("{:?}", r.0);
            println!("{:?}", r.1);
            println!("{:?}", r.2);
            println!("{:?}", r.3);
        }    
    }


    // let stream = "room-1";

    // let mut x = Streams::new(".");
    // x.add(stream, StreamId::Asterisk)?;

    // println!("{:?}", x.len(stream));
    // let range = x.range(stream, StreamId::Min, StreamId::Max, None)?;
    // println!("{:?}", range);

    // let msg = range.iter().nth(0).unwrap();
    // println!("{:?}", msg);

    // let range = x.range(stream, 
    //     StreamId::Specific(msg.timestamp, msg.order), 
    //     StreamId::Specific(msg.timestamp, msg.order), Some(1))?;
    // println!("{:?}", range);

    // let r = x.group_create(stream, "consumer-1")?;
    // println!("{:?}", r);

    // let (tx, rx) = std::sync::mpsc::channel();
    // std::thread::spawn(move || {
    //     let stream = "room-1";
    //     let mut x = Streams::new(".");
    //     tx.send(1).unwrap();
    //     let r = x.group_read("consumer-1", "a", stream);
    //     println!("group_read: {:?}", r);
    // });
    // rx.recv().unwrap();
    // std::thread::sleep(std::time::Duration::from_secs(1));
    // x.add(stream, StreamId::Asterisk)?;

    // std::thread::sleep(std::time::Duration::from_secs(1));
    // let pending = x.pending(stream, "consumer-1").unwrap();
    // println!("pending: {:?}", pending);

    // let r = x.ack(stream, "consumer-1", &pending.1);
    // println!("ack: {:?} {:?}", &pending.1, r);

    // let pending = x.pending(stream, "consumer-1");
    // println!("pending: {:?}", pending);

    Ok(())
}