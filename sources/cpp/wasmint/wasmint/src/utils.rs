use std::result::Result;

pub fn read_uleb128<T: std::io::Read>(f: &mut T) -> Result<(u32,u32), std::io::Error>
{
    let mut result = 0u32;
    let mut shift = 0u32;

    let mut buffer = [0u8;1];
    let mut bytes = 0;

    loop
    {
        f.read_exact(&mut buffer)?;
        bytes += 1;

        let byte =  buffer[0];
        let byte = byte & 0b01111111;
        result |= (byte as u32) << shift;

        if (buffer[0] & 0b10000000) == 0
        {
            break Ok((result,bytes));
        }

        shift += 7;
    }
}

pub fn foreach_uleb128<T, E, F>(f: &mut T, mut callback: F) -> Result<(), std::io::Error>
    where 
        T: std::io::Read,
        std::io::Error: std::convert::From<E>,
        F: FnMut(&mut T) -> Result<(),E>
{
    let (mut count,_) = read_uleb128(f)?;

    while count > 0
    {
        callback(f)?;
        count -= 1;
    }

    Ok(())
}

pub fn vec_uleb128<T, E, F, TElement>(f: &mut T, mut callback: F) -> Result<std::vec::Vec<TElement>, std::io::Error>
    where 
        T: std::io::Read,
        std::io::Error: std::convert::From<E>,
        F: FnMut(&mut T) -> Result<TElement,E>
{
    let (mut count,_) = read_uleb128(f)?;
    let mut v = Vec::new();

    while count > 0
    {
        let el = callback(f)?;
        v.push(el);
        count -= 1;
    }

    Ok(v)
}

pub fn read_uleb128_string<T: std::io::Read>(f: &mut T) -> Result<(String,u32), std::io::Error>
{
    let (size, size_len) = read_uleb128(f)?;
        
    let mut s = vec![0u8;size as usize];
    f.read_exact(&mut s)?;

    Ok((std::str::from_utf8(&s).unwrap().to_string(), size_len))
}

pub fn cast<T: Sized>(p: &mut *const u8) -> T
{
    unsafe 
    {
        let size = std::mem::size_of::<T>() as isize;
        let t = std::ptr::read::<T>(*p as *const T);
        *p = p.offset(size);

        t
    }
}

pub fn read_and_cast<T: Sized, TRead: std::io::Read>(f: &mut TRead) -> Result<T, std::io::Error>
{
    let size = std::mem::size_of::<T>();
    let mut buffer = vec![0u8;size];
    f.read_exact(&mut buffer)?;

    let mut ptr = buffer.as_ptr();
    Ok(cast::<T>(&mut ptr))
}

pub fn read_u8<TRead: std::io::Read>(f: &mut TRead) -> Result<u8, std::io::Error>
{
    let mut buffer = [0u8;1];
    f.read_exact(&mut buffer)?;
    Ok(buffer[0])
}

pub fn read_u32<TRead: std::io::Read>(f: &mut TRead) -> Result<u32, std::io::Error>
{
    let mut buffer = [0u8;4];
    f.read_exact(&mut buffer)?;
    Ok(u32::from_le_bytes(buffer))
}

pub fn read_f32<TRead: std::io::Read>(f: &mut TRead) -> Result<f32, std::io::Error>
{
    let mut buffer = [0u8;4];
    f.read_exact(&mut buffer)?;
    Ok(f32::from_le_bytes(buffer))
}
