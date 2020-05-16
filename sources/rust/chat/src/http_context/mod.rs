use std::error::Error;
use http_parser::*;
use multimap::MultiMap;
use strings::string_buffer::StringBuffer;
use mio::net::{TcpStream};
use std::io::{Read, Write};

struct HttpParserState
{
    finished: bool,
    method: http_parser::HttpMethod,
    url: String,
    headers: MultiMap<String, String>,
    last_header_field: StringBuffer,
    last_header_value: StringBuffer,
}

impl HttpParserState
{
    fn new () -> HttpParserState {
        HttpParserState {
            finished: false,
            method: HttpMethod::Get,
            url: "".to_owned(),
            headers: MultiMap::new(),
            last_header_field: StringBuffer::new(),
            last_header_value: StringBuffer::new(),
        }
    }

    fn push_header(&mut self)
    {
        if self.last_header_value.len > 0
        {
            self.headers.insert(self.last_header_field.to_string(), self.last_header_value.to_string());

            self.last_header_field = StringBuffer::new();
            self.last_header_value = StringBuffer::new();
        }
    }
}

impl HttpParserCallback for HttpParserState {
    fn on_message_complete(&mut self, _: &mut HttpParser) -> CallbackResult {
        self.finished = true;
        Ok(ParseAction::None)
    }

    fn on_url(&mut self, parser: &mut HttpParser, data: &[u8]) -> CallbackResult {
        self.method = parser.method.unwrap_or(HttpMethod::Get);
        self.url = std::str::from_utf8(data)
            .or(Err("invalid url"))?
            .to_owned();
        Ok(ParseAction::None)
    }

    fn on_header_field(&mut self, _: &mut HttpParser, data: &[u8]) -> CallbackResult {
        self.push_header();

        let v = std::str::from_utf8(data)
            .or(Err("invalid header name"))?
            .to_owned();

        self.last_header_field.push_str(&v);
        Ok(ParseAction::None)
    }

    fn on_header_value(&mut self, _: &mut HttpParser, data: &[u8]) -> CallbackResult {
        let v = std::str::from_utf8(data)
            .or(Err("invalid header value"))?
            .to_owned();

        self.last_header_value.push_str(&v);
        Ok(ParseAction::None)
    }

    fn on_headers_complete(&mut self, _: &mut HttpParser) -> CallbackResult {
        self.push_header();
        Ok(ParseAction::None)
    }
}

pub struct HttpContext
{
    stream: TcpStream,

    parser: HttpParser,
    state: HttpParserState,
}

impl HttpContext
{
    pub fn new(stream: TcpStream) -> HttpContext {
        HttpContext
        {
            stream: stream,
            parser: HttpParser::new(HttpParserType::Request),
            state: HttpParserState::new()
        }
    }

    pub fn read(&mut self) -> Result<bool, Box<dyn Error>>
    {
        let mut buffer = [0; 10];
        loop {
            let size = match self.stream.read(&mut buffer) {
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(0),
                x => x
            }?;
            if size == 0 { break; }

            self.parser.execute(&mut self.state, &buffer);
            if self.state.finished {
                return Ok(true);
            }

            if size < buffer.len() { break; }
        }

        return Ok(false);
    }

    pub fn write(&mut self, body: &str) -> Result<(), Box<dyn Error>>
    {
        let h = format!("Content-Length: {}\n", body.len());


        self.stream.write("HTTP/1.1 200 OK\n".as_bytes())?; 
        self.stream.write(h.as_bytes())?;
        self.stream.write("\n".as_bytes())?;
        self.stream.write(body.as_bytes())?;
        Ok(())
    }
}
