use std::error::Error;
use std::time::Duration;
use mio::net::{UdpSocket};
use mio::{Events, Interest, Poll, Token};

// In test I, the client sends a
// STUN Binding Request to a server, without any flags set in the
// CHANGE-REQUEST attribute, and without the RESPONSE-ADDRESS attribute.
// This causes the server to send the response back to the address and
// port that the request came from. 
//
// 0                   1                   2                   3
// 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |0 0|     STUN Message Type     |         Message Length        |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |                         Magic Cookie                          |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
// |                                                               |
// |                     Transaction ID (96 bits)                  |
// |                                                               |
// +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
fn test_i()
{

}

 fn WriteStunMessage(buffer: &mut std::io::Write) -> bool {
    buf->WriteUInt16(type_);
    buf->WriteUInt16(length_);
    if (!IsLegacy())
      buf->WriteUInt32(stun_magic_cookie_);
    buf->WriteString(transaction_id_);
  
    for (const auto& attr : attrs_) {
      buf->WriteUInt16(attr->type());
      buf->WriteUInt16(static_cast<uint16_t>(attr->length()));
      if (!attr->Write(buf)) {
        return false;
      }
    }
  
    return true;
  }

fn main() -> Result<(), Box<dyn Error>> {
    let mut poll = Poll::new()?;

    const SENDER: Token = Token(0);
    let mut sender_socket = UdpSocket::bind("127.0.0.1:8080".parse()?)?;
    
    poll.registry()
        .register(&mut sender_socket, SENDER, Interest::WRITABLE)?;
    sender_socket.connect("127.0.0.1:8081".parse()?)?;
    
    let mut events = Events::with_capacity(128);
    loop {
        poll.poll(&mut events, Some(Duration::from_millis(100)))?;
        for event in events.iter() {
            match event.token() {
                SENDER=>
                {
                    println!("Error: {:?}", sender_socket.take_error());

                    let msg_to_send = [9; 9];
                    let _ = sender_socket.send(&msg_to_send)?;
                },
                _ => println!("Something bizarre...")
            }
        }
    }
}
