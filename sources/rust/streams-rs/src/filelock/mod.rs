pub struct FileLock
{
    path: String
}

pub struct FileLockGuard
{
    f: std::fs::File,
}

impl Drop for FileLockGuard
{
    fn drop(&mut self)
    {
        use fs2::FileExt;
        self.f.unlock().unwrap();
    }
}

impl FileLock
{
    pub fn new(path: &str) -> Self
    {
        Self { path: path.to_string() }
    }

    pub fn lock(&mut self, name: &str) -> Result<FileLockGuard, std::io::Error>
    {
        use fs2::FileExt;

        let path = format!("{}/{}.lock", self.path, name);
        let f = std::fs::File::with_options()
            .read(true)
            .write(true)
            .truncate(true)
            .create(true)
            .open(&path)
            .unwrap();
        
        f.lock_exclusive().unwrap();

        Ok(FileLockGuard { f })
    }
}