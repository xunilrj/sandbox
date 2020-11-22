fn main() {
    use std::io::*;

    println!("{:?}", std::env::current_dir());

    let mut f = std::fs::File::create(".rust").unwrap();
    f.write_all(&[0u8]).unwrap();
    //f.sync_data().unwrap();
    drop(f);
}
