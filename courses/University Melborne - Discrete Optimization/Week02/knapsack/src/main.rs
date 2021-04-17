use std::{
    fs::File,
    io::{BufReader, Read},
    str::FromStr,
};

mod solvers;
mod utils;

#[extension_trait::extension_trait]
pub impl<T: FromStr, R: Read> ReadTFromR<T> for R {
    fn read_value(&mut self) -> std::result::Result<T, ()> {
        let mut buf = vec![];

        loop {
            let mut b = [0u8];
            self.read_exact(&mut b).unwrap();
            if b[0] == b' ' || b[0] == b'\n' {
                break;
            } else {
                buf.push(b[0]);
            }
        }

        std::str::from_utf8(buf.as_slice())
            .unwrap()
            .parse()
            .map_err(|_| ())
    }
}

#[derive(Debug)]
pub struct Item {
    weight: u64,
    value: u64,
}

fn main() {
    let args: Vec<_> = std::env::args().collect();

    let algo = args.get(1).unwrap();
    let filename = args.get(2).unwrap();
    let adjustcost = args
        .get(3)
        .map(|x| x.as_str())
        .unwrap_or("1.0")
        .parse()
        .unwrap_or(1.0);
    let printscoreonly = args
        .get(4)
        .map(|x| x.as_str())
        .unwrap_or("false")
        .parse()
        .unwrap_or(false);

    let file = File::open(filename).unwrap();
    let mut reader = BufReader::new(file);

    let qty_items: u64 = reader.read_value().unwrap();
    let mut k_cap: u64 = reader.read_value().unwrap();
    k_cap = (k_cap as f32 * adjustcost) as u64;

    let mut items = Vec::new();
    for _ in 0..qty_items {
        let value = reader.read_value().unwrap();
        let weight: u64 = reader.read_value().unwrap();
        let mut i = Item {
            value,
            weight: (weight as f32 * adjustcost) as u64,
        };

        if (adjustcost - 1.0).abs() > 0.01 {
            i.weight += 1;
        }

        items.push(i);
    }

    let (opt, max, items) = match algo.as_str() {
        "dp" => solvers::dp::solve(k_cap, items.as_slice()),
        "branchbound" => solvers::branchbound::solve(k_cap, items.as_slice()),
        "greedy" => solvers::greedy::solve(k_cap, items.as_slice()),
        _ => panic!("Unkown algo"),
    };

    println!(
        "{} {}",
        max,
        if opt && (adjustcost - 1.0).abs() < 0.01 {
            "1"
        } else {
            "0"
        }
    );
    if !printscoreonly {
        for i in items {
            print!("{} ", i);
        }
    }
    println!("");
}
