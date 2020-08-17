use core::marker::PhantomData;
use core::ops::Deref;
use core::ops::DerefMut;
use std::mem::transmute;

struct SimpleWrapper<T1, T2>(T1, PhantomData<T2>);
impl<T1, T2> Deref for SimpleWrapper<T1, T2> {
    type Target = T1;
    fn deref(&self) -> &<Self as std::ops::Deref>::Target {
        &self.0
    }
}
impl<T1, T2> DerefMut for SimpleWrapper<T1, T2> {
    fn deref_mut(&mut self) -> &mut <Self as std::ops::Deref>::Target {
        &mut self.0
    }
}

struct Connection {
    data: i32,
}
impl Connection {
    fn new() -> ConnectionState<Open> {
        SimpleWrapper::<Connection, Open>(Connection { data: 0 }, Default::default())
    }
}
struct Open;
struct Close;

type ConnectionState<T> = SimpleWrapper<Connection, T>;

impl ConnectionState<Open> {
    fn close(mut self) -> ConnectionState<Close> {
        self.data += 1;
        unsafe { std::mem::transmute(self) }
    }
}

transition! {Open => Close,
    fn close(conn: &mut Connection) {
        self.data += 1;
    }
}

fn main() {
    let conn = Connection::new();
    let conn = conn.close();
    //conn.close();
}
