pub mod tempfile;

use std::path::{Path, PathBuf};

use arrow::array::UInt8Array;
use async_stream::stream;

use futures_core::stream::Stream;

pub fn file_read_all<P: Into<PathBuf>>(
    path: P,
    buffer_size: usize,
) -> impl Stream<Item = std::io::Result<UInt8Array>> + 'static {
    let path: PathBuf = path.into();
    use tokio::io::AsyncReadExt;
    stream! {
        let mut f = tokio::fs::File::open(path).await?;
        let mut buffer = vec![0u8; 16 * 1024];

        let mut builder = UInt8Array::builder(buffer_size);
        loop {
            let mut builder_qty = 0;

            loop {
                let qty = f.read(&mut buffer).await?;
                if qty == 0 {
                    break;
                }

                builder.append_slice(&buffer[0..qty]).unwrap();
                builder_qty += qty;
                if builder_qty >= buffer_size {
                    break;
                }
            }

            if builder_qty == 0 {
                break;
            } else {
                let array = builder.finish();
                yield Ok(array);
            }
        }
    }
}

#[tokio::test]
pub async fn file_read_all_test() {
    use futures_util::StreamExt;
    let file_size = 1024 * 1024 + 1;
    let f = tempfile::TempFile::random(file_size).expect("Cannot create tempfile");
    let values = file_read_all(&f.path);

    let mut count = 0;
    pin_mut!(values);
    while let Some(Ok(items)) = values.next().await {
        count += items.len();
    }

    assert_eq!(count, file_size);
}
