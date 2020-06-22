# Step 1 - REPL

```
use std::io::*;

fn exec(cmd: &str) -> bool {
    if cmd == ".exit" {
        return false;
    } else {
        println!("Unkown command: [{}]", cmd);
    }
    return true;
}

fn main() {
    loop {
        print!("> ");
        std::io::stdout()
            .flush()
            .expect("Problem to access console");
        let mut line = String::new();
        let run = match stdin().read_line(&mut line) {
            Ok(size) => exec(&line[0..size - 1]),
            Err(e) => {
                println!("{:?}", e);
                false
            }
        };
        if !run {
            break;
        }
    }
}

```