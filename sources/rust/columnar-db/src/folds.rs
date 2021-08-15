use arrow::{
    array::{PrimitiveArray, UInt8Array},
    datatypes::ArrowPrimitiveType,
};
use futures_core::stream::Stream;
use futures_util::{pin_mut, stream::StreamExt};

pub async fn max<
    T: ArrowPrimitiveType,
    E,
    S: Stream<Item = std::result::Result<PrimitiveArray<T>, E>>,
>(
    stream: S,
) -> Option<<T as ArrowPrimitiveType>::Native>
where
    <T as ArrowPrimitiveType>::Native: Ord,
{
    let mut current_max = None;
    pin_mut!(stream);
    while let Some(Ok(items)) = stream.next().await {
        let new_max = items.iter().max().flatten();
        current_max = match (current_max, new_max) {
            (None, None) => None,
            (None, Some(new_max)) => Some(new_max.clone()),
            (Some(old_max), None) => Some(old_max),
            (Some(old_max), Some(new_max)) => Some(old_max.max(new_max)),
        };
    }
    current_max
}
