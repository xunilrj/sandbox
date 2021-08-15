use futures_core::stream::Stream;
use futures_util::{pin_mut, stream::StreamExt};

pub async fn to_channel<T, S: Stream<Item = T>>(stream: S, channel: flume::Sender<T>) {
    pin_mut!(stream);
    while let Some(item) = stream.next().await {
        if let Err(_) = channel.send_async(item).await {
            break;
        }
    }
}

pub async fn to_new_channel<T, S: Stream<Item = T>>(stream: S) -> flume::Receiver<T> {
    let (s, r) = flume::unbounded();

    pin_mut!(stream);
    while let Some(item) = stream.next().await {
        if let Err(_) = s.send_async(item).await {
            break;
        }
    }

    r
}
