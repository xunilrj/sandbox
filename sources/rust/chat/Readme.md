# Metal Chat

Chats have become the "hello world" standard for real-time applications. In this tutorial we intend to follow this "tradition" writing the most scalable possible solution, to our skills, going as low level as possible in terms of OS calls. That is why we are calling this "Metal Chat".

## Stack

We will first start using Rust, a low-level language that has a very pleasant high-level langauge feeling. Later, we will replicate the code using moden C++.

We will not touch here how to install Rust. See here (https://rustup.rs/) if you need help.

## Init

First thing we need to navigate to the folder that will contain our code.

```
cd <SOMETHING>
cargo init
cargo build
```

Now we have our application initialized and, luckly, is compiling.

## The First Poll

To better understand why modern networks applications follow a "loop" architecture see here (TODO: link). To the hasty, it allows us to "multiplex" our I/O channels. Which allows us to serve more clients with the same amount of resources. Remeber: effiency keeps the boss happy.

So, to start our loop we will use a lib/crate called https://github.com/tokio-rs/mio. Mio implements the "loop" using the best avaiable APIs for each OS: epoll for Unix, kqueue for OSX, and IO-Ports for Windows. And deliver them to us through an unified API.

All we need to do is import "mio" in the "cargo.toml"

```
[dependencies]
mio = {version = "0.7.0", features = ["os-poll"]}
```

All we need to do now is to create the loop.

```
use mio::{Poll};

fn main()
{
    let poll = Poll::new();
}
```

This code does absolutely nothing, yet, off course. But, the most intriguing fact for non-Rust developers is that "::new()" does not returns a new poll. No! It returns a "std::result::Result<mio::poll::Poll, std::io::Error>".

What this means is that the return is a "mio::poll::Poll" OR a "std::io::Error". Before having access to the "Poll" we need to check if any error ocurred. We have multiple ways to do this.

1 - Easiest: just call "https://doc.rust-lang.org/std/result/enum.Result.html#method.unwrap"

```
let poll = Poll::new().unwrap();
```

"unwrap" returns the value IF the "Result<>" contains one. Otherwise will panic, which in Rust parlance means an exception. If you read the "Result" documentation I linked above, you will learn this method is discouraged. Although a lot of tutorials use it, because it is the easiest method.

2 - using if

This is the most obvious code that any developer would create. It works, but is verbose. Pretty quick your code will be 99% just error treatment. 

If your are wondering "return ()" is the same as "return;" for "void functions".

```
if(r.is_ok()) { poll = r.unwrap(); }
else { return (); }
```

3 - The Rust-onic way

For these cases Rust has a special way that generates a secure and readable code. And you type less (sometimes). It is the "?" operator. The description below describes it, and it is pretty much what we did above.

```
The ? is shorthand for the entire match statements 
we wrote earlier. In other words, ? applies to a 
Result value, and if it was an Ok, it unwraps it 
and gives the inner value. If it was an Err, 
it returns from the function you're currently in. 
```
https://doc.rust-lang.org/edition-guide/rust-2018/error-handling-and-panics/the-question-mark-operator-for-easier-error-handling.html

The advantage now is that our code becomes:

```
use mio::{Poll};

fn main()
{
    let poll = Poll::new()?;
}
```

The problem is... now our code does not compiles. Rust error are normally pretty descriptive, but they are generally verbose. Let's try to decode it.

```
error[E0277]: the `?` operator can only be used in a function that returns `Result` or `Option` (or another type that implements `std::ops::Try`)
 --> src/main.rs:5:16
  |
3 | / fn main()
4 | | {
5 | |     let poll = Poll::new()?
  | |                ^^^^^^^^^^^^ cannot use the `?` operator in a function that returns `()`
6 | | }
  | |_- this function should return `Result` or `Option` to accept `?`
  |
  = help: the trait `std::ops::Try` is not implemented for `()`
  = note: required by `std::ops::Try::from_error`
```

The first thing, you can search the error code here: https://doc.rust-lang.org/error-index.html. In this case, the error code does not help. Luckly the error description is more helpful.

```
the `?` operator can only be used in a function that returns `Result`
```

This case is so common that Rust has a specific page for it:
https://doc.rust-lang.org/nightly/edition-guide/rust-2018/error-handling-and-panics/question-mark-in-main-and-tests.html

Basically out "main" also needs to return "Result", because the "?" operator not only returns on error, but it propagates the error. Easy enough. Our main does not return anything, "()" = void, and it can return an "Error". So:

```
use mio::{Poll};

fn main() -> Result<(), std::error::Error>
{
    let poll = Poll::new()?;
}
```

Now we have another compile error.

```
error[E0277]: the size for values of type `(dyn std::error::Error + 'static)` cannot be known at compilation time
 --> src/main.rs:3:14
  |
3 | fn main() -> Result<(), std::error::Error>
  |              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ doesn't have a size known at compile-time
  |
  = help: the trait `std::marker::Sized` is not implemented for `(dyn std::error::Error + 'static)`
  = note: to learn more, visit <https://doc.rust-lang.org/book/ch19-04-advanced-types.html#dynamically-sized-types-and-the-sized-trait>
  = note: required by `std::result::Result`
```

What this means?

When we search for "std::error::Error" in documentation we found this link.
https://doc.rust-lang.org/std/error/trait.Error.html

```
Error is a trait
```

You do not need to fully understand Rust traits here. Just think of them as interfaces, abstract classes. Can you return by value an interface? an abstract class? No, off course. Because you don't know its size.

In C++ you could return by reference/pointer. But this would create two problems:
1 - reference/pointer to a stack allocated;
2 - or heap allocated memory that would never by deallocated.

In modern C++, you could return a smart-pointer, more precisely a unique_ptr (https://en.cppreference.com/w/cpp/memory/unique_ptr). Well, for this case Rust has the "Box" (https://doc.rust-lang.org/std/boxed/struct.Box.html).

So let us try again.

```
use mio::{Poll};

fn main() -> Result<(), std::boxed::Box<std::error::Error>>
{
    let poll = Poll::new()?;
}
```

If we compile now we receive the following warning:

```
warning: trait objects without an explicit `dyn` are deprecated
 --> src/main.rs:3:41
  |
3 | fn main() -> Result<(), std::boxed::Box<std::error::Error>>
  |                                         ^^^^^^^^^^^^^^^^^ help: use `dyn`: `dyn std::error::Error`
  |
  = note: `#[warn(bare_trait_objects)]` on by default
```

It is just asking us to put "dyn" before the type inside Box.


```
use mio::{Poll};

fn main() -> Result<(), std::boxed::Box<dyn std::error::Error>>
{
    let poll = Poll::new()?;
}
```

If we compile now, we receive yet another error:

```
error[E0308]: mismatched types
 --> src/main.rs:3:14
  |
3 | fn main() -> Result<(), std::boxed::Box<dyn std::error::Error>>
  |    ----      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expected enum `std::result::Result`, found `()`
  |    |
  |    implicitly returns `()` as its body has no tail or `return` expression
  |
  = note:   expected enum `std::result::Result<(), std::boxed::Box<(dyn std::error::Error + 'static)>>`
          found unit type `()`
```

It states that our function must return a "Result" but it is returning () "void". When a function returns "Result" you can return using "Ok" or "Err".

```
use mio::{Poll};

fn main() -> Result<(), std::boxed::Box<dyn std::error::Error>>
{
    let poll = Poll::new()?;
    return Ok(());
}
```

And it compiles. It will complain that we are not using the "poll" variable but that is fine. We will.

First let us simplify this code using the "use" keyword.

```
use mio::{Poll, Events};
use std::{error::Error, boxed::Box};

fn main() -> Result<(), Box<dyn Error>>
{
    let poll = Poll::new()?;
    let events = Events::with_capacity(128);

    return Ok(());
}
```

So now we, finally, have out "Poll" created. What this struct thus is to "poll" the OS and tell us what happened since the last "poll". It will communicate with us in form of events. To accomodate these events we will need a list of events.

```
use mio::{Poll, Events};

fn main() -> Result<(), std::boxed::Box<dyn std::error::Error>>
{
    let poll = Poll::new()?;
    let events = Events::with_capacity(128);
    return Ok(());
}
```

Now we are able to "see" up to 128 events between calls to the "Pool" to pool the OS. See more at https://docs.rs/mio/0.7.0/mio/event/struct.Events.html.

If we have more events they will return immediatelly in the next call, as per the documentation. So we do not need to go crazy here.

```
At most events.capacity() events will be returned. If 
there are further pending readiness events, they will 
be returned on the next call to poll.
```
https://docs.rs/mio/0.7.0/mio/struct.Poll.html

So now we start "poll"ing our "Pool".

```
use mio::{Poll, Events};
use std::{error::Error, boxed::Box};

fn main() -> Result<(), Box<dyn Error>>
{
    let poll = Poll::new()?;
    let events = Events::with_capacity(128);

    poll.poll(events, None);

    return Ok(());
}
```

The first parameter it is the events list that "Poll" will populate and the second is the timeout. Here we do not have a timeout, so it will wait forever.

Even before compiling let us reason what this method will do. We will try to anticipate a compile error.

We know that "Poll" will modify the event list and we will read this list later. So it does not make send to pass "event" by value, or in Rust parlance, nor to move "event" to another place. We need to pass by reference.

Let us compile and see what the Rust compiler thinks:

```
error[E0308]: mismatched types
 --> src/main.rs:9:15
  |
9 |     poll.poll(events, None);
  |               ^^^^^^
  |               |
  |               expected `&mut mio::event::events::Events`, found struct `mio::event::events::Events`
  |               help: consider mutably borrowing here: `&mut events`

error: aborting due to previous error
```

Yeah! He agrees with us.

```
expected `&mut mio::event::events::Events`, found struct `mio::event::events::Events`
```

He is asking for a reference, "&" operator. More specifically he is asking for "&mut" operator. It makes sense, we know this method will mutate the list. So we try &mut" allowing the "poll" function to mutate the event list.


```
use mio::{Poll, Events};
use std::{error::Error, boxed::Box};

fn main() -> Result<(), Box<dyn Error>>
{
    let poll = Poll::new()?;
    let events = Events::with_capacity(128);

    poll.poll(&mut events, None);

    return Ok(());
}
```

The problem is that, Rust declare variable immutable by default. We must allow variables to be mutable.

```
error[E0596]: cannot borrow `events` as mutable, as it is not declared as mutable
 --> src/main.rs:9:15
  |
7 |     let events = Events::with_capacity(128);
  |         ------ help: consider changing this to be mutable: `mut events`
8 | 
9 |     poll.poll(&mut events, None);
  |               ^^^^^^^^^^^ cannot borrow as mutable
```

We do this when we declare a variable.

```
use mio::{Poll, Events};
use std::{error::Error, boxed::Box};

fn main() -> Result<(), Box<dyn Error>>
{
    let poll = Poll::new()?;
    let mut events = Events::with_capacity(128);

    poll.poll(&mut events, None);

    return Ok(());
}
```

If we compile now we get the following error:

```
error[E0596]: cannot borrow `poll` as mutable, as it is not declared as mutable
 --> src/main.rs:9:5
  |
6 |     let poll = Poll::new()?;
  |         ---- help: consider changing this to be mutable: `mut poll`
...
9 |     poll.poll(&mut events, None);
  |     ^^^^ cannot borrow as mutable
```

The same problem, but now for the "poll" variable. It does make sense. We will also mutate the "poll". So we fix this with the same solution. The difference is that we do not need to modify anything in the call.

```
use mio::{Poll, Events};
use std::{error::Error, boxed::Box};

fn main() -> Result<(), Box<dyn Error>>
{
    let mut poll = Poll::new()?;
    let mut events = Events::with_capacity(128);

    poll.poll(&mut events, None);

    return Ok(());
}
```

Now out application is compiling again. Probably complaining we are not using the return of the "poll" method. We will do this now. 

The "poll" function as we can see in the documentation also returns a "Result". So we will simplify things here and use the "?" directly.
https://docs.rs/mio/0.7.0/mio/struct.Poll.html#method.poll

```
use mio::{Poll, Events};
use std::{error::Error, boxed::Box};

fn main() -> Result<(), Box<dyn Error>>
{
    let mut poll = Poll::new()?;
    let mut events = Events::with_capacity(128);

    let r = poll.poll(&mut events, None)?;

    return Ok(());
}
```

In this case "r" is "()" because the "poll" function returns "Result<()>". We do not need to specify it as variable.

```
use mio::{Poll, Events};
use std::{error::Error, boxed::Box};

fn main() -> Result<(), Box<dyn Error>>
{
    let mut poll = Poll::new()?;
    let mut events = Events::with_capacity(128);

    poll.poll(&mut events, None)?;

    return Ok(());
}
```

Now the application is compiling, and without warnings. Fine! We are ready to run our application. We can do this with "carog run"

```
# cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
    Running `target/debug/chat`
```

And the application nerver returns. Why?
Remember that we passed "None" as timeout. "poll" will never timeout, and thinking about it, it will never return. Because we have never said what it should "poll" or "wait" in the OS.

So it is basically "polling" nothing and waiting forever. Almost as nihillistically adventure as Ishmael in Moby Dick. Let us give some raison d'etre for this Pool.

## Polling a TCP connection

The tell "Poll" what it needs to "poll" we need to use its "registry". 
https://docs.rs/mio/0.7.0/mio/struct.Registry.html

```
use std::{error::Error, boxed::Box};
use mio::{Poll, Events};

fn main() -> Result<(), Box<dyn Error>>
{
    let mut poll = Poll::new()?;
    let registry = poll.registry();
    let mut events = Events::with_capacity(128);

    poll.poll(&mut events, None)?;

    return Ok(());
}
```

To use it we just need to call the "register" method. But before we need something to register in side the registry. We will start with a simple TCP server.

Luckly "mio" has one already done for us. We just need to enable it in the cargo.toml.

```
[dependencies]
mio = {version = "0.7.0", features = ["os-poll","tcp"]}
```

and use it inside our code.

```
use std::{error::Error, boxed::Box};
use mio::{Poll, Events};
use mio::net::{TcpListener};

fn main() -> Result<(), Box<dyn Error>>
{
    let mut poll = Poll::new()?;
    let registry = poll.registry();
    let mut events = Events::with_capacity(128);

    let addr = "127.0.0.1:8080".parse()?;
    let mut server = TcpListener::bind(addr)?;

    poll.poll(&mut events, None)?;

    return Ok(());
}
```

Now we are to register our server. This can very easily be done as:

```
use std::{error::Error, boxed::Box};
use mio::{Poll, Events, Token, Interest};
use mio::net::{TcpListener};

fn main() -> Result<(), Box<dyn Error>>
{
    let mut poll = Poll::new()?;
    let mut events = Events::with_capacity(128);

    let addr = "127.0.0.1:8080".parse()?;
    let mut server = TcpListener::bind(addr)?;
    const SERVERTOKEN: Token = Token(0);
    poll.registry().register(&mut server, SERVERTOKEN, Interest::READABLE)?;

    poll.poll(&mut events, None)?;

    return Ok(());
}
```

Token will be used by "mio" to tell us "who" has generated an event. And "READABLE" we are telling "mio" to only poll for "read" events. In this case our server is in "listen-only" (could this be the ultimate women dream? ;))

Now we are ready to "cargo run" again.

```
# cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
    Running `target/debug/chat`
```

In another shell we try:

```
# telnet 127.0.0.1 8080
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
Connection closed by foreign host.
```

And our application closes. Wonderful! It works.

Wel... it works, but it is useless. Let us implement our loop to keep listening and echoing what we receive.

## The Loop

Now we will put our "poll" inside a loop and allow the server to run, luckly, forever. We will also stopping show all the code and focus only on the canges from now on.

```
fn main() -> Result<(), Box<dyn Error>>
{
    ...

    loop
    {
        println!("Waiting...");
        poll.poll(&mut events, None)?;

        for event in events.iter() {
            println!("Event received.");
        }
    }
}
```

If we "cargo run" now we will see something interesting.

```
# cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
    Running `target/debug/chat`
Waiting...
```

If we connect:

```
# telnet 127.0.0.1 8080
Trying 127.0.0.1...
Connected to 127.0.0.1.
Escape character is '^]'.
```

we will see

```
# cargo run
   Compiling chat v0.1.0 (/root/github/sandbox/sources/rust/chat)
    Finished dev [unoptimized + debuginfo] target(s) in 0.30s
     Running `target/debug/chat`
Waiting...
Event received.
Waiting...
```

The problem here is that "telnet" is starting the TCP connection, but we are not accepting it. So that is why the "telnet" is in that "hang" position.

TCP 3-Way Handshake (SYN,SYN-ACK,ACK)
https://www.inetdaemon.com/tutorials/internet/tcp/3-way_handshake.shtml

So let us accept all connections.

```
for event in events.iter() {
    println!("Event received.");

    if event.token() == SERVERTOKEN {
        println!("SERVERTOKEN");
        let (mut conn,addr) = server.accept()?;
    }
}
```

If we "cargo run" and "telnet" our server as we did above. We will still see the connection dropped. Why? The answer is simple, we are letting the "conn" which is the connection going out of scope. This means that Rust will automatically close the connection for us. Other wise we would be leaking connection and memory here.

So we need a way to promote this connection. To keep it alive. The easiest way, for now is to create two variables outside the loop.


```
let mut connection: None;
let mut address: None;

loop
{
    ...
}
```

Back to our "accept" code we will now set these variables.

```
for event in events.iter() {
    println!("Event received.");

    if event.token() == SERVERTOKEN {
        println!("SERVERTOKEN");
        let (mut conn,addr) = server.accept()?;

        connection = Some(dbg!(conn));
        address = Some(dbg!(addr));
    }
}
```

"dbg!" is a helper macro (like a function) that will spit something to the console and return a value.
"Some" here is a pattern known to use instead of null. Its use will become clerar in a second.

Now our server will print:

```
Waiting...
Event received.
SERVERTOKEN
[src/main.rs:37] conn = TcpStream {
    addr: V4(
        127.0.0.1:8080,
    ),
    peer: V4(
        127.0.0.1:53056,
    ),
    fd: 5,
}
[src/main.rs:38] addr = V4(
    127.0.0.1:53056,
)
Waiting...
```

Beautiful. The connection is accepted.

If you keep typing in the telnet and pressing enter you will notice that nothing happend inside our server. Why? Because we are not polling this new connection. We need to register it to the poll. 

And the process is pretty much what we did before. Let us also remove some of the "println!".

CONNTOKEN is another token we need to create.

```
const CONNTOKEN: Token = Token(1);
let mut connection = None;
let mut address = None;

...

for event in events.iter() {
    if event.token() == SERVERTOKEN {
        let (mut conn,addr) = server.accept()?;

        poll.registry()
            .register(&mut conn, CONNTOKEN, Interest::READABLE)?;

        connection = Some(dbg!(conn));
        address = dbg!(addr);
    } else {
        println!("Unkown token");
    }
}
```

Now we are receiving the event for this new connection and seeing:

```
Waiting...
Event received.
Unkown token
```

Let us now treat this event.

If the token is about this last accepted connection and the "connection" variable is set. We will do something.

```
if event.token() == SERVERTOKEN {
    ...
}
else if event.token() == CONNTOKEN && connection.is_some() {
    let mut buffer = [0; 10];

    let mut conn = connection.take().unwrap();
    loop {
        let size = conn.read(&mut buffer)?;
        println!("Read: {}", size);
        dbg!(buffer);

        if(size < buffer.len()) { break; }
    }
}
else {
    ...
}
```

Let us analyze these lines more carefully.

```
let mut buffer = [0; 10];
```

Here we are zero-initializing a 10 elements fixed size array.
https://doc.rust-lang.org/std/primitive.array.html  


```
let mut conn = connection.take().unwrap();
```

Remember that our connection in wrapped inside a struct called "Option". It is kown pattern that allows a safer use then "null". If we had null, we could use the "connection" variable and incur in a error. 

Throw the first stone who never fell into a Null Exception.

With the "Option" struct it is impossible to use the value inside of it without first checking if there is a value. Another option, as "Result" is just "unwrap" the value and cross your fingers that the "Option" has a value inside.

In our case here, remember that we already test if the "Option" has a value in the "if" clause. So we are safe here.

The problem comes when you cimbine this with a Rust feature that can only existe one variable to a object. So if you "unwrap" an "Option" you kill this "Option". Othersie both "Option" and our current code would "point" to the connection object.

One option is to "unwrap" the "Option", use the connection, and "wrap" it again.

Here we will do something different just for the sake of it. We will get the value inside of the option and put None there, use our connection and let it die, basically closing the connection. If Rust allowed multiple "pointers" to the same object, this would be a very dangerous operation. We could be killing an object in use in another place. But we can safely do this here because we know that nobody else is pointing to this object. +1 for Rust here.

Luckily we have the "take" function that do this "swap" for us.
https://doc.rust-lang.org/std/option/enum.Option.html#method.take

```
let mut conn = connection.take().unwrap();
```

Now "connection" is "None" again and "conn" points, and it is the unique "pointer", to our connection. We can very easily read from the connection.

```
let size = conn.read(&mut buffer)?;
println!("Read: {}", size);
dbg!(buffer);
```

and, off course, we loop whislt we have data to read.

```
loop {
    let size = conn.read(&mut buffer)?;
    println!("Read: {}", size);
    dbg!(buffer);

    if size < buffer.len() { break; }
}
```

If we "cargo run" now we will see.

```
Waiting...
Event received.
Read: 10
[src/main.rs:47] buffer = [
    49,
    50,
    51,
    52,
    53,
    54,
    55,
    56,
    57,
    49,
]
Read: 4
[src/main.rs:47] buffer = [
    50,
    51,
    13,
    10,
    53,
    54,
    55,
    56,
    57,
    49,
]
Waiting...
```

Which is perfect. I sent 14 bytes and all of them we read and the connection was closed because "connection" went out of scope. Beautiful!

The problem here, off course, is that we let a bug in out application. What happen when I send a payload that its size is a multiple of the buffer size. Let us try with 10 bytes.

```
Waiting...
Event received.
Read: 10
[src/main.rs:47] buffer = [
    49,
    50,
    51,
    52,
    53,
    54,
    55,
    56,
    13,
    10,
]
Error: Os { code: 11, kind: WouldBlock, message: "Resource temporarily unavailable" }
```

We first read the 10 bytes. Brilliant! But what is this error? And worst, our application is done. The server process was killed.

The problem is that "mio" is an async I/O lib. Instead of blocking, it give us an error stating that it does not have data. In this particular case we can just ignore. The "error" is expected.

```
WouldBlock
The operation needs to block to complete, but the blocking operation was requested to not occur.
https://doc.rust-lang.org/nightly/std/io/enum.ErrorKind.html#WouldBlock.v
```

This problem goes even deeper. In a multi thread scenario it is possible that when you start to read something for the first time, it was already read by someone else. Read the doc here:

```
Spurious events
Poll::poll may return readiness events even if the associated event source is not actually ready. Given the same code, this may happen more on some platforms than others. It is important to never assume that, just because a readiness event was received, that the associated operation will succeed as well.

If operation fails with WouldBlock, then the caller should not treat this as an error, but instead should wait until another readiness event is received.
https://docs.rs/mio/0.7.0/mio/struct.Poll.html#method.poll
```

Long story short. We need to be prepared to these "errors".

First let us focus on the "conn.read" function. The documentation tells me that the "read" function returns an "Result". So it is my "?" after it that is propagating the "Error" and killing my server.

fn read(&mut self, buf: &mut [u8]) -> Result<usize>  
https://doc.rust-lang.org/nightly/std/io/trait.Read.html#tymethod.read

So let remove it for now.


```
loop {
    let r = conn.read(&mut buffer);
    if r.is_err() { break; }

    let size = r.unwrap();
    println!("Read: {}", size);
    dbg!(buffer);

    if size < buffer.len() { break; }
}
```

If we send 10 bytes again, now we see:

```
Waiting...
Event received.
Read: 10
[src/main.rs:50] buffer = [
    49,
    50,
    51,
    52,
    53,
    54,
    55,
    56,
    13,
    10,
]
Waiting...
```

Much better.

Our first version of the loop is ready. Let us try to improve the code.

## "match" versus "if"

A Rust developer is probably calling me all names now for treating the error like we did above. That is because Rust has other language constructs that enable us to have a cleaner erro treatment. Let us try:

The first obvious improvement would be using Rust "match" (https://doc.rust-lang.org/rust-by-example/flow_control/match.html).

```
match conn.read(&mut buffer) {
    Err(_) => break,
    Ok(size) => {
        println!("Read: {}", size);
        dbg!(buffer);

        if size < buffer.len() { break; }
    }
}
```

Second would be to move this code to another function:


```
fn read(conn: &mut TcpStream)
{
    let mut buffer = [0; 10];
    loop {
        match conn.read(&mut buffer) {
            Err(_) => break,
            Ok(size) => {
                println!("Read: {}", size);
                dbg!(buffer);

                if size < buffer.len() { break; }
            }
        }
    }
}
```

and change our loop

```
 else if event.token() == CONNTOKEN && connection.is_some() {
    let mut conn = connection.take().unwrap();
    read(&mut conn);
}
```

Another improvement is to use some of the "fundamentals" functions of "Option". For example, we do not want to test if the "Option" has a value or not.
What we really want is:

1 - If there is a value, call the function "read" with its value;
2 - If there is no value, do nothing.

We can achieve this using: "map", for example.


```
connection.take().as_mut().map(read);
```

The "as_mut" is needed, because our function "read" receive a "& mut".

We can even simplyfy our code even more. We do not need to check if the "Option" is empty or not. The code below is enterily safe.

```
...
else if event.token() == CONNTOKEN {
    connection.take().as_mut().map(read);
}
```

Read this line again and see how much we are doing one just one line of code:

1 - We are taking ownership of the connection;
2 - We are completely reading its buffers;
3 - We are closing the connection.

The connection management semantic is completely implicit. If this is good or not, I let your style decide.

We can continue our refactor exporting the accept code to its method. No new concepts here.

```
fn accept(poll: &Poll, server: &TcpListener) -> Result<(TcpStream, SocketAddr, Token), Box<dyn Error>>
{
    let (mut conn,addr) = server.accept()?;
    const CONNTOKEN: Token = Token(1);

    poll.registry()
        .register(&mut conn, CONNTOKEN, Interest::READABLE)?;

    Ok((conn, addr, CONNTOKEN))
}
```

our loop becomes. We are ignoring the error for now.

```
let et = event.token();
if et == SERVERTOKEN {
    match accept(&poll, &server) {
        Ok((conn, _, token)) => {
            connection = Some(conn);
            CONNTOKEN = Some(token);
        },
        Err(_) => {}
    }
}
else if CONNTOKEN.map_or(false, |x| x == et) {
    connection.take().as_mut().map(read);
}
else {
    println!("Unkown token");
}
```

The problem now is that the connection TOKEN is created inside the accept method.

So outside the loop we need to define two variables:

```
let mut CONNTOKEN = None;
let mut connection = None;
```

Now that "CONNTOKEN" is an "Option" is a little bit harder to compare its value. Remember that "mio" give us a token indicating what happened. We need to discover from whom that token came from.

If you try to compare the event token with the Option. The compiler will quickly complain.

```
error[E0308]: mismatched types
  --> src/main.rs:66:27
   |
66 |             else if et == CONNTOKEN {
   |                           ^^^^^^^^^ expected struct `mio::token::Token`, found enum `std::option::Option`
   |
   = note: expected struct `mio::token::Token`
                found enum `std::option::Option<mio::token::Token>`
```

One way is to use another "fundamental" function from "Option".

map_or
https://doc.rust-lang.org/std/option/enum.Option.html#method.map_or

```
else if CONNTOKEN.map_or(false, |x| x == et) {
        connection.take().as_mut().map(read);
}
```

We can improve the readability here creating a function to the "Option" struct. In C# you can create "Extensions Methods". Basically they are static methods that pretend to be instance methods. They are very userfull for small helper/utilizy methods.

Extension Methods (C# Programming Guide)
https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/extension-methods

We can do something very similar in Rust. It is a little bit more involved, though.

First we need to come back to traits. Think here in "interfaces". Just a set of methods that an object must implement.

Then we show how that interface can be applied to an already existing object, here "Option".

We are also constraining the type to have implemented the PartialEq trait.

```
trait OptionEqual<T> where T : PartialEq
{
    fn is_equal(&self, v: &T) -> bool;
}

impl<T: std::cmp::PartialEq> OptionEqual<T> for Option<T>
{
    fn is_equal(&self, v: &T) -> bool
    {
        self.as_ref().map_or(false, |x| x.eq(v))
    }
}
```

Now we can use it in our loop:

```
for event in events.iter() {
    println!("Event received.");

    let et = event.token();
    if et == SERVERTOKEN {
        match accept(&poll, &server) {
            Ok((conn, _, token)) => {
                connection = Some(conn);
                CONNTOKEN = Some(token);
            },
            Err(_) => {}
        }
    }
    else if CONNTOKEN.is_equal(&et) {
        connection.take().as_mut().map(read);
    }
    else {
        println!("Unkown token");
    }
}
```

## Client Map

We went a long road until now, but we are still storing our client in a single variable. We need to upgrade the solution to a collection.

We already know that "mio" will giveus a plain integer. The logical solution is to use a map/dictionary here to connect this plain integer to a context, where we can continue the logic of serving this client. In our case here, we will read until we find a break-line and then we will print what we received. It does not matter the size of the line (dangerous!) and then we will put some limit to the line size.

First thing is we can delete out "CONNTOKEN" and "connection" variables. Now we are going to have a Hashmap.

```
use std::{error::Error, boxed::Box, net::SocketAddr, collections::HashMap};
...
let mut streams = HashMap::new();
```

Another modification is when we accept the connection and how we read from the connection.

```
if et == SERVERTOKEN {
    match accept(&poll, &server) {
        Ok((conn, _, token)) => { streams.insert(token.0, conn); },
        Err(_) => {}
    }
    continue;
}

match streams.get_mut(&et.0) {
    Some(conn) => { read(conn); },
    None => println!("Unkown token")
}
```

We have two problem here. One is a bug we created. In theory is possible for a client request arrive between the time we registered the connection in the "Poll" and the time we inserted it on the "HashMap". We will only treat this later.

Another small problem, is that now, this server has a slightly different behaviour than our previous. Can you spot the difference?

Yes! The connection is no longer closed after the client send its message. Althought this is probably desired, I prefer to "fix" this first.

What is keeping the connection alive now, is, off course, the "HashMap". All we need is to remove the connection from it, and let the connection die.

```
 match streams.get_mut(&et.0) {
    Some(conn) => { read(conn); streams.remove(&et.0); },
    None => println!("Unkown token")
}
```

Ok! Now we are back to where we were.

But we still need to fix a small problem. All tokens are being returned with value 1.

```
let mut ctxs = HashMap::new();
let mut tokeni = 0;
```

```
if et == SERVERTOKEN {
    tokeni += 1;
    match accept(tokeni, &poll, &server) {
        Ok((conn, _, token)) => { ctxs.insert(token.0, HttpContext::new(conn)); },
        Err(_) => {}
    }
    continue;
}
```

Now we can change the read method to print a string when we find the newline inside the buffer. Our "read" function is a little bit more complex than before. We have a lot to unpack here.


```
fn read(conn: &mut TcpStream) -> Result<(), Box<dyn Error>>
{
    let mut buffer = [0; 10];
    loop {
        let size = match conn.read(&mut buffer) {
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(0),
            x => x
        }?;
        if size == 0 { break; }

        let pos = buffer.iter()
            .position(|&x| x == 13 || x == 10)
            .unwrap_or(buffer.len());
        let end_str = min(size, pos);

        if end_str > 0 {
            let line = std::str::from_utf8(&buffer[0..end_str])?;
            println!("{}", line);
        }

        if size < buffer.len() { break; }
    }

    return Ok(());
}
```

First thing is:

```
let size = match conn.read(&mut buffer) {
    Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(0),
    x => x
}?;
if size == 0 { break; }
```

Before we were returning an error everytime "read" also return an error. Altough this makes sense, we know that "WouldBlock" is expected. So instead of failing our "read", everytime we receive an "WouldBlock" error I transform this error in the value "0". And after this we use the "?". Now, we only propagate unexpected errors.

And if the size is zero, nothing to read, we break the loop.

```
let pos = buffer.iter()
    .position(|&x| x == 13 || x == 10)
    .unwrap_or(buffer.len());
let end_str = min(size, pos);
```

After this we look for a breakline (ASCII code 13) inside the buffer. Remember that we never clear the buffer, so we can find an old breakline. I just guarantee that we return or the break line before the position that we wrote now (from 0 to index "size") or we return 0, meaning we have not read a breakline.

```
if end_str > 0 {
    let line = std::str::from_utf8(&buffer[0..end_str])?;
    println!("{}", line);
}
```

I print the line only if a new breakline was found.

Now we are ready to actually parse the Http Request.

## Parsing the Http Request

Up until now we have being saving just the TcpStream for each connection. To parse the http request we will need more context. In our case here we will be reading the request in chunks of 10 bytes at a time and sending them to the parser.

So let first start creating our context struct.

```
struct HttpContext
{
    stream: TcpStream,

    parser: HttpParser,
    state: HttpParserState,
}
```
Secondly we will go to where we insert the accepted connection to the hashmap

```
let parser = HttpParser::new(HttpParserType::Request);
streams.insert(token.0, HttpContext {
    parser: parser,
    stream: conn,

    state: HttpParserState::new()
});
```

"streams" is name that does not makes sense anymore. Let us change to "ctxs".


```
let parser = HttpParser::new(HttpParserType::Request);
ctxs.insert(token.0, HttpContext {
    parser: parser,
    stream: conn,

    state: HttpParserState::new()
});
```

Now we can export this to a function called "new".

```
impl HttpContext
{
    fn new(stream: TcpStream) -> HttpContext {
        HttpContext
        {
            stream: stream,
            parser: HttpParser::new(HttpParserType::Request),
            state: HttpParserState::new()
        }
    }
}
```

In the end our "accept" code is:

```
 if et == SERVERTOKEN {
    match accept(&poll, &server) {
        Ok((conn, _, token)) => { ctxs.insert(token.0, HttpContext::new(conn)); },
        Err(_) => {}
    }
    continue;
}
```

Not bad, but we are still gonna leave the "Err" without log.

Now we need to change our read function. We no longer are just printing the request, we are sending everything we read to a HttpParser. This parser will signal me that it has finished by its "finished" field.

The magic happens in the "execute" method. First I pass the "HttpParserState" to the execute. The "ctx.parser" is the actual parser, the "ctx.state" is the request and what was built up until now. This separation allows the parser to generate multiple messages.

```
fn read(ctx: &mut HttpContext) -> Result<(), Box<dyn Error>>
{
    let mut stream = &mut ctx.stream;
    let mut buffer = [0; 10];
    loop {
        let size = match stream.read(&mut buffer) {
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(0),
            x => x
        }?;
        if size == 0 { break; }

        ctx.parser.execute(&mut ctx.state, &buffer);
        if ctx.state.finished {
        }

        if size < buffer.len() { break; }
    }

    return Ok(());
}
```

To test the code up to here I will "dbg!" the "ctx.state" and see what we are parsing.


```
if ctx.state.finished {
    println!("{}", ctx.state.method.to_string());
    println!("{}", ctx.state.url);
    dbg!(&ctx.state.headers);
}
```

We can "cargo run" our server and test it with "wget http://127.0.0.1:8080/asd"

It was a long run, but our server is almost ready to answer the request.

```
Waiting...
Event received.
GET
/asd
[src/main.rs:128] &ctx.state.headers = {
    "Connection": [
        "Keep-Alive",
    ],
    "Accept": [
        "*/*",
    ],
    "User-Agent": [
        "Wget/1.20.3 (linux-gnu)",
    ],
    "Accept-Encoding": [
        "identity",
    ],
    "Host": [
        "127.0.0.1:8080",
    ],
}
```

So let us dive deeper in the HttpParser.

We are using a Rust port of the nodejs HttpParser (https://github.com/magic003/http-parser-rs). It works with a callback. You pass the http data as you can, and it will call an object with which has parsed so far.

The callbacks are pretty straightfoward.

```
impl HttpParserCallback for HttpParserState {
    fn on_message_complete(&mut self, _: &mut HttpParser) -> CallbackResult {
        self.finished = true;
        Ok(ParseAction::None)
    }
```

With this callback, we know when the message is completely parsed.

```
fn on_url(&mut self, _: &mut HttpParser, data: &[u8]) -> CallbackResult {
    self.method = parser.method.unwrap_or(HttpMethod::Get);
    self.url = std::str::from_utf8(data)
        .or(Err("invalid url"))?
        .to_owned();
    Ok(ParseAction::None)
}
```

With this callback, we know the method and the URL on the first line. 

Headers, for some reason, are a little bit harder. First "on_header_field" and "on_header_value" are called multiple times for the same header.
So we accumulate its name in the "self.last_header_field". When the header name is finished, it will not call the callback, it will call the "on_header_value". We will also accumulate the header value.

Now two things can happen. Or we are back to "on_header_field" to next header. when we push our previous header in a MultiMap;  
or the header parse is complete, where we push our previous header.

```
fn on_header_field(&mut self, _: &mut HttpParser, data: &[u8]) -> CallbackResult {
    self.push_header();

    let v = std::str::from_utf8(data)
        .or(Err("invalid header name"))?
        .to_owned();

    self.last_header_field.push_str(&v);
    Ok(ParseAction::None)
}

fn on_header_value(&mut self, _: &mut HttpParser, data: &[u8]) -> CallbackResult {
    let v = std::str::from_utf8(data)
        .or(Err("invalid header value"))?
        .to_owned();

    self.last_header_value.push_str(&v);
    Ok(ParseAction::None)
}

fn on_headers_complete(&mut self, _: &mut HttpParser) -> CallbackResult {
    self.push_header();
    Ok(ParseAction::None)
}
```

And that is all we need to parse the http request.

## Modules

The problem now is that the code is already big. We have 200 lines of code that do very different things. Let us try to break them.

Starting with the HttpContext.

To create our module we need to create an folder.

```
> cd src
> mkdir http_context
> cd http_context
> touch mod.rs
```

After this we can basically control+c, control+v the code for:

```
struct HttpParserState {...}
impl HttpParserState {...}
impl HttpParserCallback for HttpParserState {...}
struct HttpContext {...}
impl HttpContext {...}
```

We also moved the "read" function to inside the "impl "

```
impl HttpContext
{
    pub fn read(&mut self) -> Result<(), Box<dyn Error>>
    {
        ...
    }
}
```

on the "main.rs" we need to import and use the module.

```
mod http_context;
use http_context::{HttpContext};
```

Later we change how the "ctx" read the stream.

```
 match ctxs.get_mut(&et.0) {
    Some(ctx) => { let _ = ctx.read(); ctxs.remove(&et.0); },
    None => println!("Unkown token")
}
```

Given that we now moved the code to another "module" we need to expose some parts of it using "pub".


```
pub struct HttpContext {...}

impl HttpContext
{
    pub fn new(stream: TcpStream) -> HttpContext {...}
    pub fn read(&mut self) -> Result<(), Box<dyn Error>> {...}
}
```

## Generating Response

To start generating a Response, we will make the read method return the HttpContext, as soon as it finish the parsing.

