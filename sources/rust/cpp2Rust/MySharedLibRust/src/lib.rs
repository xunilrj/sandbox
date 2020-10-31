use serde;
use serde::{Serialize, Deserialize};

fn str_from_size_buffer(name_size: usize, name: *const u8) -> Option<&'static str>
{
    let name = unsafe { std::slice::from_raw_parts(name, name_size)};
    std::str::from_utf8(name).map_or(None,Some)
}

#[derive(Serialize, Deserialize, Debug)]
struct PrepareInstallModule { }

#[repr(u64)]
enum ContentType
{
    Json = 1,
    Bson = 2
}

fn parse_message(content_type: u64, msg_type: u64, len: usize, data: *const u8) -> Messages
{
    let msg_content_type = unsafe{std::mem::transmute::<u64, ContentType>(content_type)};
    let buffer = unsafe { std::slice::from_raw_parts(data, len)};
    match msg_type
    {
        1 => 
        {
            let buffer = unsafe { std::str::from_utf8_unchecked(buffer) };
            Messages::PrepareInstallModule(serde_json::from_str::<PrepareInstallModule>(buffer).unwrap())
        },
        _ => Messages::Unknown (buffer)
    }
}

#[derive(Debug)]
enum Messages
{
    Unknown (&'static [u8]),
    PrepareInstallModule(PrepareInstallModule)
}

#[no_mangle]
pub extern "C" fn print_hello_from_rust(msg_content_type: u64,
    msg_type: u64,
    buffer_size: usize,
    buffer: *const u8,
) {
    println!("Message: {:?} {:?} {:?} {:?}", msg_content_type, msg_type, buffer_size, buffer);
    let msg = parse_message(msg_content_type, msg_type, buffer_size, buffer);
    println!("Message: {:?}", msg);
}
