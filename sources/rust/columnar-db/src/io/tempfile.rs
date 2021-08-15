use std::io::Write;

pub struct TempFile {
    pub path: std::path::PathBuf,
}

impl TempFile {
    pub fn random(mut size: usize) -> std::io::Result<Self> {
        let mut path = std::env::temp_dir();
        let filename = format!("{}", rand::random::<u64>());
        path.push(filename);

        let mut file = std::fs::File::create(&path)?;

        let buffer_size = 4 * 1024;
        let mut data = vec![0u8; buffer_size];
        while size > buffer_size {
            for i in 0..buffer_size {
                data[i] = rand::random::<u8>();
            }
            size -= buffer_size;
            file.write_all(&data)?;
        }

        for i in 0..size {
            data[i] = rand::random::<u8>();
        }
        file.write_all(&data[0..size])?;

        file.sync_all()?;

        Ok(Self { path: path.into() })
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(self.path.as_path());
    }
}
