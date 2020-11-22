pub struct MMBox<T: 'static + Sized>
{
    value: &'static mut T
}

impl<T: Sized> MMBox<T>
{
    pub fn new(f: &mut std::fs::File) -> Self
    {
        use std::ffi::c_void;
        use std::os::unix::io::AsRawFd;
        use nix::sys::mman::*;

        let size = std::mem::size_of::<T>();
        let value = unsafe {
            mmap(std::ptr::null_mut::<c_void>(),
            size,
            ProtFlags::PROT_READ | ProtFlags::PROT_WRITE,
            MapFlags::MAP_SHARED,
            f.as_raw_fd(),
            0).unwrap()
        };
        let value = unsafe {
            std::mem::transmute::<*mut c_void, &'static mut T>(value)
        };

        Self { value }
    }

    pub fn sync(&mut self) -> Result<(), ()>
    {
        use std::ffi::c_void;
        use nix::sys::mman::*;
        let size = std::mem::size_of::<T>();
        unsafe 
        {
            let r = msync(self.value as *mut T as *mut c_void, size, MsFlags::MS_SYNC);
            if r.is_err()
            {
                return Err(());
            }
        }

        Ok(())
    }
}

impl<T: Sized> std::convert::AsRef<T> for MMBox<T>
{
    fn as_ref(&self) -> &T {
        self.value
    }
}

impl<T: Sized> std::convert::AsMut<T> for MMBox<T>
{
    fn as_mut(&mut self) -> &mut T {
        self.value
    }
}

impl<T: Sized>  std::ops::Deref for MMBox<T>
{
    type Target = T;
    fn deref(&self) -> &Self::Target
    {
        self.value
    }
}

impl<T: Sized>  std::ops::DerefMut for MMBox<T>
{
    fn deref_mut(&mut self) -> &mut Self::Target
    {
        self.value
    }
}

impl<T: Sized>  Drop for MMBox<T>
{
    fn drop(&mut self) {
        use std::ffi::c_void;
        use nix::sys::mman::*;
        let size = std::mem::size_of::<T>();
        unsafe 
        {
            let _ = munmap(self.value as *mut T as *mut c_void, size);
        }
    }
}
