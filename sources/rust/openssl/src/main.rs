use openssl::ssl::SslFiletype;

fn main() {
    use openssl::ssl::{SslConnector, SslMethod};
    use std::io::{Read, Write};
    use std::net::TcpStream;

    let mut builder = SslConnector::builder(SslMethod::tls()).unwrap();
    builder.set_verify(openssl::ssl::SslVerifyMode::NONE);
    builder
        .set_certificate_file("clientcertificate.pem", SslFiletype::PEM)
        .unwrap();
    builder
        .set_private_key_file("clientkey.pem", SslFiletype::PEM)
        .unwrap();
    let connector = builder.build();

    let stream = TcpStream::connect("localhost:8443").unwrap();
    let mut stream = connector.connect("localhost.com", stream).unwrap();

    stream.write_all(b"GET / HTTP/1.1\r\n\r\n").unwrap();
    let mut res = vec![];
    stream.read_to_end(&mut res).unwrap();
    println!("{}", String::from_utf8_lossy(&res));
}
