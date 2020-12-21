use derive_builder::*;
use std::{cmp::Ordering, fmt::Display};
use url::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Uri(String);

impl std::cmp::PartialEq<str> for Uri {
    fn eq(&self, other: &str) -> bool {
        self.0.as_str() == other
    }
}

impl Uri {
    fn dump_to_write(&self, w: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        w.write_all(self.0.to_string().as_bytes())?;
        Ok(())
    }
}

impl std::convert::From<String> for Uri {
    fn from(value: String) -> Self {
        Uri(value)
    }
}

impl Display for Uri {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}

impl Default for Uri {
    fn default() -> Self {
        Self("/".to_string())
    }
}

impl std::convert::TryFrom<&str> for Uri {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self(value.to_string()))
    }
}

#[derive(Debug)]
pub enum Error {
    UriNotValid,
}

impl std::convert::From<ParseError> for Error {
    fn from(_: ParseError) -> Self {
        Error::UriNotValid
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RequestMethod {
    Get,
    Post,
    Put,
    Delete,
    Other(String),
}

impl Default for RequestMethod {
    fn default() -> Self {
        RequestMethod::Get
    }
}

impl From<String> for RequestMethod {
    fn from(value: String) -> Self {
        let value = value.to_lowercase();
        match value.as_str() {
            "get" => RequestMethod::Get,
            "post" => RequestMethod::Post,
            "put" => RequestMethod::Put,
            "delete" => RequestMethod::Delete,
            _ => RequestMethod::Other(value),
        }
    }
}

impl RequestMethod {
    fn dump_to_write(&self, w: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        let str = match self {
            RequestMethod::Get => "GET",
            RequestMethod::Post => "POST",
            RequestMethod::Put => "PUT",
            RequestMethod::Delete => "DELETE",
            RequestMethod::Other(s) => s.as_str(),
        };
        w.write_all(str.as_bytes())
    }
}

// pub trait Request {
//     type BodyReader<'a>: std::io::Read;
//     type BodyWriter<'a>: std::io::Write;
//     type HeaderIter<'a>: std::iter::Iterator<Item = (&'a String, &'a Vec<String>)>;

//     fn method(&self) -> &RequestMethod;
//     fn method_mut(&mut self) -> &mut RequestMethod;
//     fn uri(&self) -> &Uri;
//     fn uri_mut(&mut self) -> &mut Uri;

//     fn iter_headers<'a>(&'a self) -> Self::HeaderIter<'a>;
//     fn push_header(&mut self, name: &str, value: &str);

//     fn remove_header(&mut self, name: &str) -> Option<Vec<String>>;
//     fn header(&self, name: &str) -> Option<&String>;

//     fn body_writer<'a>(&'a mut self) -> &mut Self::BodyWriter<'a>;

//
// }

#[derive(Default, Builder, Debug)]
#[builder(try_setter, setter(into))]
pub struct HttpRequest {
    method: RequestMethod,
    uri: Uri,
    #[builder(default = "std::collections::HashMap::new()")]
    headers: std::collections::HashMap<String, Vec<String>>,
    #[builder(default = "std::vec::Vec::new()")]
    body: Vec<u8>,
}

impl HttpRequest {
    // impl Request for HttpRequest {
    //     type HeaderIter<'a> = std::collections::hash_map::Iter<'a, String, Vec<String>>;
    //     type BodyReader<'a> = std::io::Cursor<&'a [u8]>;
    //     type BodyWriter<'a> = std::vec::Vec<u8>;

    fn method(&self) -> &RequestMethod {
        &self.method
    }
    fn method_mut(&mut self) -> &mut RequestMethod {
        &mut self.method
    }

    fn uri(&self) -> &Uri {
        &self.uri
    }
    fn uri_mut(&mut self) -> &mut Uri {
        &mut self.uri
    }

    fn iter_headers<'a>(&'a self) -> std::collections::hash_map::Iter<'a, String, Vec<String>> {
        self.headers.iter()
    }

    fn push_header(&mut self, name: &str, value: &str) {
        match self.headers.get_mut(name) {
            Some(v) => {
                v.push(value.to_owned());
            }
            None => {
                self.headers.insert(name.to_owned(), vec![value.to_owned()]);
            }
        }
    }

    pub fn header(&self, name: &str) -> Option<&str> {
        self.headers.get(name)?.get(0).map(|x| x.as_str())
    }

    fn header_cmp(&self, name: &str, value: &str) -> bool {
        match self.headers.get(name) {
            Some(v) => {
                if v.len() == 1 {
                    let v = v.get(0).unwrap();
                    v.as_str().cmp(value) == Ordering::Equal
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn header_value<'a, T: std::str::FromStr>(&'a self, name: &str) -> Option<T> {
        let v = self.header(name)?;
        match T::from_str(v) {
            Ok(v) => Some(v),
            _ => None,
        }
    }

    fn push_content_length(&mut self) {
        self.push_header("Content-Length", &format!("{}", self.body_length()));
    }

    //     fn remove_header(&mut self, name: &str) -> Option<Vec<String>> {
    //         self.headers.remove(name)
    //     }

    fn body_writer(&mut self) -> &mut Vec<u8> {
        &mut self.body
    }

    //     fn body_reader<'a>(&'a mut self) -> Self::BodyReader<'a> {
    //         std::io::Cursor::new(&self.body.as_slice())
    //     }

    fn body_length(&self) -> usize {
        self.body.len()
    }

    fn body_reader<'a>(&'a mut self) -> std::io::Cursor<&'a [u8]> {
        std::io::Cursor::new(self.body.as_slice())
    }

    fn body_dump_to_write<T: std::io::Write>(&mut self, w: &mut T) -> Result<(), std::io::Error> {
        let mut r = self.body_reader();
        let _ = std::io::copy(&mut r, w)?;
        Ok(())
    }

    fn dump_to_write(&mut self, w: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        self.method.dump_to_write(w)?;
        w.write_all(" ".as_bytes())?;
        self.uri.dump_to_write(w)?;
        w.write_all(" HTTP/1.1".as_bytes())?;
        w.write_all("\r\n".as_bytes())?;

        for (k, v) in self.iter_headers() {
            for v in v.iter() {
                w.write_all(k.as_bytes())?;
                w.write_all(": ".as_bytes())?;
                w.write_all(v.as_bytes())?;
                w.write_all("\r\n".as_bytes())?;
            }
        }

        w.write_all("\r\n".as_bytes())?;
        self.body_dump_to_write(w)?;

        Ok(())
    }

    pub fn try_parse_from_cursor(
        cursor: &mut std::io::Cursor<&[u8]>,
    ) -> Result<Self, std::io::Error> {
        let mut b = HttpRequestBuilder::default();

        b.method(cursor.read_string_until_char(' ' as u8)?);
        cursor.read_many(' ' as u8)?;
        b.uri(cursor.read_string_until_char(' ' as u8)?);
        cursor.read_string_until_char('\r' as u8)?;
        let _ = cursor.read_once("\r\n");

        let mut b = b.build().unwrap();
        while !cursor.peek_once("\r\n")? {
            let line = cursor.read_string_until_char('\r' as u8)?;
            let colon = line.find(":").unwrap();
            let (name, value) = line.split_at(colon);
            let value = value.trim_start_matches(":").trim_start_matches(" ");

            cursor.read_once("\r\n").unwrap();

            b.push_header(name, value);
        }

        cursor.read_once("\r\n").unwrap();

        match b.header_value::<usize>("Content-Length") {
            Some(content_length) => {
                let mut buffer = vec![0u8; content_length];
                use std::io::Read;
                cursor.read_exact(&mut buffer).unwrap();

                let w = b.body_writer();
                use std::io::Write;
                w.write_all(buffer.as_slice()).unwrap();
            }
            _ => {}
        }

        Ok(b)
    }
}

impl std::convert::TryFrom<&[u8]> for HttpRequest {
    type Error = std::io::Error;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        let mut cursor = std::io::Cursor::new(value);
        Self::try_parse_from_cursor(&mut cursor)
    }
}

impl std::convert::TryFrom<&Vec<u8>> for HttpRequest {
    type Error = std::io::Error;

    fn try_from(value: &Vec<u8>) -> Result<Self, Self::Error> {
        let mut cursor = std::io::Cursor::new(value.as_slice());
        Self::try_parse_from_cursor(&mut cursor)
    }
}

impl std::convert::TryFrom<Vec<u8>> for HttpRequest {
    type Error = std::io::Error;

    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        let mut cursor = std::io::Cursor::new(value.as_slice());
        Self::try_parse_from_cursor(&mut cursor)
    }
}
// impl HttpRequest {

// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResponseStatus {
    Ok,
    Accepted,
    BadRequest,
    NotFound,
    Other(u16),
}

impl Default for ResponseStatus {
    fn default() -> Self {
        ResponseStatus::Ok
    }
}

impl Display for ResponseStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            ResponseStatus::Ok => "OK",
            ResponseStatus::BadRequest => "BadRequest",
            ResponseStatus::Accepted => "Accepted",
            ResponseStatus::NotFound => "Not Found",
            ResponseStatus::Other(status) => "",
        };
        f.write_str(str)
    }
}

impl ResponseStatus {
    fn dump_to_write(&self, w: &mut impl std::io::Write) -> Result<usize, std::io::Error> {
        let str = match self {
            ResponseStatus::Ok => "200",
            ResponseStatus::Accepted => "201",
            ResponseStatus::BadRequest => "400",
            ResponseStatus::NotFound => "404",
            ResponseStatus::Other(status) => "XXX",
        };
        w.write_all(str.as_bytes())?;
        Ok(str.len())
    }
}

impl From<String> for ResponseStatus {
    fn from(value: String) -> Self {
        let value = value.to_lowercase();
        match value.as_str() {
            "200" => ResponseStatus::Ok,
            "201" => ResponseStatus::Accepted,
            "400" => ResponseStatus::BadRequest,
            "404" => ResponseStatus::NotFound,
            _ => ResponseStatus::Other(value.parse::<u16>().unwrap()),
        }
    }
}

// pub trait Response {
//     type HeaderIter<'a>: std::iter::Iterator<Item = (&'a String, &'a Vec<String>)>;
//     type BodyReader<'a>: std::io::Read;
//     type BodyWriter<'a>: std::io::Write;

//     fn status(&self) -> &ResponseStatus;
//     fn status_mut(&mut self) -> &mut ResponseStatus;

//     fn iter_headers<'a>(&'a self) -> Self::HeaderIter<'a>;
//     fn push_header(&mut self, name: &str, value: &str);

//     fn body_reader<'a>(&'a mut self) -> Self::BodyReader<'a>;
//     fn body_length(&self) -> usize;
//     fn body_writer<'a>(&'a mut self) -> &mut Self::BodyWriter<'a>;

// }

#[derive(Default, Builder, Debug)]
#[builder(try_setter, setter(into))]
pub struct HttpResponse {
    status: ResponseStatus,
    #[builder(default = "std::collections::HashMap::new()")]
    headers: std::collections::HashMap<String, Vec<String>>,
    #[builder(default = "std::vec::Vec::new()")]
    body: Vec<u8>,
}

// impl Response for HttpResponse {
//     type HeaderIter<'a> =
//     type BodyReader<'a> = std::io::Cursor<&'a [u8]>;
//     type BodyWriter<'a> = std::vec::Vec<u8>;

// }

impl HttpResponse {
    pub fn status(&self) -> &ResponseStatus {
        &self.status
    }

    pub fn status_mut(&mut self) -> &mut ResponseStatus {
        &mut self.status
    }

    pub fn iter_headers<'a>(&'a self) -> std::collections::hash_map::Iter<'a, String, Vec<String>> {
        self.headers.iter()
    }

    pub fn header_value<'a, T: std::str::FromStr>(&'a self, name: &str) -> Option<T> {
        let v = self.header(name)?;
        match T::from_str(v) {
            Ok(v) => Some(v),
            _ => None,
        }
    }

    pub fn push_header(&mut self, name: &str, value: &str) {
        match self.headers.get_mut(name) {
            Some(v) => {
                v.push(value.to_owned());
            }
            None => {
                self.headers.insert(name.to_owned(), vec![value.to_owned()]);
            }
        }
    }

    pub fn push_content_length(&mut self) {
        self.push_header("Content-Length", &format!("{}", self.body_length()));
    }

    pub fn header(&self, name: &str) -> Option<&String> {
        self.headers.get(name)?.get(0)
    }

    pub fn body_writer(&mut self) -> &mut Vec<u8> {
        &mut self.body
    }

    pub fn body_reader<'a>(&'a self) -> std::io::Cursor<&'a [u8]> {
        std::io::Cursor::new(&self.body.as_slice())
    }

    pub fn body_length(&self) -> usize {
        self.body.len()
    }

    pub fn parse_from_read<T: std::io::Read>(r: &mut T) -> Result<HttpResponse, std::io::Error> {
        let mut b = HttpResponse::default();

        let mut buffer = [0u8; 1024];
        let size = r.read(&mut buffer)?;

        let mut cursor = std::io::Cursor::new(&buffer[0..size]);

        cursor.read_string_until_char(' ' as u8)?;
        cursor.read_many(' ' as u8)?;

        *b.status_mut() = cursor.read_string_until_char(' ' as u8)?.into();
        cursor.read_many(' ' as u8)?;

        cursor.read_string_until_char('\r' as u8)?;

        cursor.read_once("\r\n");

        while !cursor.peek_once("\r\n")? {
            let line = cursor.read_string_until_char('\r' as u8)?;
            let colon = line.find(":").unwrap();
            let (name, value) = line.split_at(colon);
            let value = value.trim_start_matches(":").trim_start_matches(" ");

            cursor.read_once("\r\n").unwrap();

            b.push_header(name, value);
        }

        cursor.read_once("\r\n").unwrap();

        match b.header_value::<usize>("Content-Length") {
            Some(content_length) => {
                let mut buffer = vec![0u8; content_length];
                use std::io::Read;
                cursor.read_exact(&mut buffer).unwrap();

                let w = b.body_writer();
                use std::io::Write;
                w.write_all(buffer.as_slice()).unwrap();
            }
            _ => {}
        }

        Ok(b)
    }

    pub fn dump_to_write(&self, w: &mut impl std::io::Write) -> Result<usize, std::io::Error> {
        let mut qtd = 0;

        let buf = "HTTP/1.1 ".as_bytes();
        w.write_all(buf)?;
        qtd += buf.len();

        let status = self.status();
        qtd += status.dump_to_write(w)?;

        let buf = format!(" {}\r\n", status);
        let buf = buf.as_bytes();
        w.write_all(buf)?;
        qtd += buf.len();

        for (k, v) in self.iter_headers() {
            for v in v.iter() {
                let buf = k.as_bytes();
                w.write_all(buf)?;
                qtd += buf.len();

                let buf = ": ".as_bytes();
                w.write_all(buf)?;
                qtd += buf.len();

                let buf = v.as_bytes();
                w.write_all(buf)?;
                qtd += buf.len();

                let buf = "\r\n".as_bytes();
                w.write_all(buf)?;
                qtd += buf.len();
            }
        }

        let buf = "\r\n".as_bytes();
        w.write_all(buf)?;
        qtd += buf.len();

        qtd += self.body_dump_to_write(w)?;

        Ok(qtd)
    }

    pub fn body_dump_to_write(&self, w: &mut impl std::io::Write) -> Result<usize, std::io::Error> {
        let mut r = self.body_reader();
        let qtd = std::io::copy(&mut r, w)?;
        Ok(qtd as usize)
    }
}

impl std::convert::TryFrom<Vec<u8>> for HttpResponse {
    type Error = std::io::Error;

    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        let mut cursor = std::io::Cursor::new(value.as_slice());
        HttpResponse::parse_from_read(&mut cursor)
    }
}

#[extension_trait::extension_trait(pub)]
impl ReadUntilChar for std::io::Cursor<&[u8]> {
    // fn read_until_char(&mut self, c: u8) -> Result<Vec<u8>, std::io::Error> {
    //     use std::io::Read;

    //     let mut v = Vec::new();

    //     loop {
    //         let mut byte = [0u8; 1];
    //         self.read_exact(&mut byte)?;
    //         if byte[0] != c {
    //             v.push(byte[0]);
    //         } else {
    //             break;
    //         }
    //     }

    //     Ok(v)
    // }

    fn read_string_until_char(&mut self, c: u8) -> Result<String, std::io::Error> {
        use std::io::Read;

        let mut v = String::new();

        loop {
            let mut byte = [0u8; 1];
            self.read_exact(&mut byte)?;
            if byte[0] != c {
                v.push(byte[0] as char);
            } else {
                break;
            }
        }

        self.set_position(self.position() - 1);

        Ok(v)
    }

    fn read_many(&mut self, c: u8) -> Result<usize, std::io::Error> {
        use std::io::Read;
        let mut count = 0usize;

        loop {
            let mut byte = [0u8; 1];
            self.read_exact(&mut byte)?;
            if byte[0] == c {
                count += 1;
            } else {
                break;
            }
        }

        self.set_position(self.position() - 1);

        Ok(count)
    }

    fn read_once(&mut self, s: &str) -> Result<(), ()> {
        let s = s.as_bytes();

        use std::io::Read;
        let mut i = 0usize;

        while i < s.len() {
            let mut byte = [0u8; 1];
            self.read_exact(&mut byte).unwrap();
            if byte[0] == s[i] {
                i += 1;
            } else {
                break;
            }
        }

        if i >= s.len() {
            Ok(())
        } else {
            Err(())
        }
    }

    fn peek_once(&mut self, s: &str) -> Result<bool, std::io::Error> {
        let pos = self.position();

        let s = s.as_bytes();

        use std::io::Read;
        let mut i = 0usize;

        while i < s.len() {
            let mut byte = [0u8; 1];
            self.read_exact(&mut byte).unwrap();
            if byte[0] == s[i] {
                i += 1;
            } else {
                break;
            }
        }

        self.set_position(pos);

        if i >= s.len() {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn as_string(&mut self) -> String {
        use std::io::Read;

        let mut string = String::with_capacity(100);
        let _ = self.read_to_string(&mut string);
        string
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn http_request_builder_and_debug() {
        // Build Request
        use std::io::Write;
        let mut request = HttpRequestBuilder::default()
            .method(RequestMethod::Post)
            .try_uri("http://www.machinaaurum.com")
            .unwrap()
            .build()
            .unwrap();
        request.push_header(
            "User-Agent",
            "Mozilla/4.0 (compatible; MSIE5.01; Windows NT)",
        );
        request.body_writer().write_all("BODY".as_bytes()).unwrap();
        request.push_content_length();

        // Dump to bytes and parse it again
        use std::convert::TryFrom;
        let mut bytes = Vec::new();
        request.dump_to_write(&mut bytes).unwrap();
        let mut request2 = HttpRequest::try_from(bytes).unwrap();

        // Check is equal the original
        assert_eq!(request2.method(), &RequestMethod::Post);
        assert_eq!(request2.uri(), "http://www.machinaaurum.com");
        assert_eq!(request2.headers.len(), 2);
        assert_eq!(
            request2.header("User-Agent").unwrap(),
            "Mozilla/4.0 (compatible; MSIE5.01; Windows NT)"
        );
        assert_eq!(request2.header("Content-Length").unwrap(), "4");
        assert_eq!(request2.body_reader().as_string(), "BODY");
    }

    #[test]
    fn http_respose() {
        use std::io::Write;
        let mut response = HttpResponseBuilder::default()
            .status(ResponseStatus::Ok)
            .build()
            .unwrap();
        response.push_header("Content-Type", "plaint/text");
        response.body_writer().write_all("BODY".as_bytes()).unwrap();
        response.push_content_length();

        // Dump to bytes and parse it again
        use std::convert::TryFrom;
        let mut bytes = Vec::new();
        response.dump_to_write(&mut bytes).unwrap();
        let response2 = HttpResponse::try_from(bytes).unwrap();

        //Check is equal the original
        assert_eq!(response2.status(), &ResponseStatus::Ok);
        assert_eq!(response2.headers.len(), 2);
        assert_eq!(response2.header("Content-Type").unwrap(), "plain/text");
        assert_eq!(response2.header("Content-Length").unwrap(), "4");
        assert_eq!(response2.body_reader().as_string(), "BODY");
    }
}
