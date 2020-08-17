use futures::stream::Stream;

trait Parser<T> {
    fn parse<'a, S: Stream<Item = T>>(
        &self,
        s: &'a mut S,
    ) -> std::result::Result<(T, &'a mut S), i32>;
}

struct Result<T: Clone>(T);
impl<T: Clone> Parser<T> for Result<T> {
    fn parse<'a, S: Stream<Item = T>>(
        &self,
        s: &'a mut S,
    ) -> std::result::Result<(T, &'a mut S), i32> {
        Ok((self.0.clone(), s))
    }
}

fn result<T: Clone>(value: T) -> Result<T> {
    Result(value)
}

fn main() {
    let r = result(1);
    println!("Hello, world!");
}
