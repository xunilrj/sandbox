fn main() {
    use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod, SslStream};
    use std::io::{Read, Write};
    use std::net::TcpListener;
    use std::net::TcpStream;
    use std::sync::Arc;
    use std::thread;

    let mut acceptor = SslAcceptor::mozilla_intermediate(SslMethod::tls()).unwrap();
    acceptor.set_verify(
        openssl::ssl::SslVerifyMode::FAIL_IF_NO_PEER_CERT | openssl::ssl::SslVerifyMode::PEER,
    );
    acceptor
        .set_private_key_file("key.pem", SslFiletype::PEM)
        .unwrap();
    acceptor
        .set_certificate_chain_file("certificate.pem")
        .unwrap();
    acceptor.check_private_key().unwrap();
    let acceptor = Arc::new(acceptor.build());

    let listener = TcpListener::bind("0.0.0.0:8443").unwrap();

    fn handle_client(mut stream: SslStream<TcpStream>) {
        stream.write_all(b"HTTP/1.1 200 OK\r\n\r\n").unwrap();
    }

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let acceptor = acceptor.clone();
                thread::spawn(move || {
                    let stream = acceptor.accept(stream).unwrap();
                    handle_client(stream);
                });
            }
            Err(e) => { /* connection failed */ }
        }
    }
}
