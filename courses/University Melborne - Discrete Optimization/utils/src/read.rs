use std::io::Read;
use std::str::FromStr;

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