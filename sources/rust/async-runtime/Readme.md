# Writing an Async Runtime

In this tutorial we are going to write an async runtime from the scratch. It does not need to be the best runtime ever. Actually, it does not need to be a good runtime. It just needs to work.

For that we need an async API. And for that we will use ```epoll``` to write a simple tcp server.

## Minimal Epoll Server

### Epoll Creation
To start out server we will do.

```
> cargo init async-runtime
```

And add libc to its dependencies

```toml
[package]
name = "async-runtime"
version = "0.1.0"
edition = "2021"

[dependencies]
libc = "0.2.126"
```

The first thing we are going to do is create an ```epoll```. We will not discuss here why ```epoll``` are better. But they are simple to create.

```
epoll_create() creates an epoll(7) instance. Since Linux 2.6.8, the size argument is ignored, but must be greater than zero; see NOTES below.
epoll_create() returns a file descriptor referring to the new epoll instance.
When no longer required, the file descriptor returned by epoll_create() should be closed by using close(2)
```
https://linux.die.net/man/2/epoll_create1  

```
close() closes a file descriptor
```
https://linux.die.net/man/2/close

Well. A warning. We will use A LOT of unsage, given that we are calling the OS directly here.
So our first line of code is:

```rust
fn main() {
    let epoll_fd = unsafe { libc::epoll_create1(1024) };
    unsafe { libc::close(epoll_fd) };
}
```

So there you have it. Simple.
But also disturbingly C-ish like.

We don't wanna forget to close the ```epoll```. So let us wrap it and drop it correctly.

```rust
use std::os::raw::c_int;

pub struct Epoll(c_int);

impl Epoll {
    pub fn new() -> Self {
        let fd = unsafe { libc::epoll_create1(1024) };
        Self(fd)
    }
}

impl Drop for Epoll {
    fn drop(&mut self) {
        unsafe { libc::close(self.0) };
    }
}

fn main() {
    let epoll = Epoll::new();
}
```

Much nicer, but we need to check is the ```epoll``` was created correctly. So we do:

```rust
#[derive(Debug)]
pub enum EpollErrors {
    CannotBeCreated,
}

impl Epoll {
    pub fn new() -> Result<Self, EpollErrors> {
        let fd = unsafe { libc::epoll_create1(1024) };
        if fd == -1 {
            Err(EpollErrors::CannotBeCreated)
        } else {
            Ok(Self(fd))
        }
    }
}
```

On main we do:

```rust
fn main() {
    let epoll = Epoll::new().unwrap();
}
```

and when we run:

```rust
> cargo run
thread 'main' panicked at 'called `Result::unwrap()` on an `Err` value: CannotBeCreated', src/main.rs:28:30
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

Which is nice, but still not super useful. What went wrong? ```epoll```documentation says the following:

```
Return Value
On success, these system calls return a nonnegative file descriptor. On error, -1 is returned, and errno is set to indicate the error.
Errors
EINVAL
size is not positive.
EINVAL
(epoll_create1()) Invalid value specified in flags.
EMFILE
The per-user limit on the number of epoll instances imposed by /proc/sys/fs/epoll/max_user_instances was encountered. See epoll(7) for further details.
ENFILE
The system limit on the total number of open files has been reached.
ENOMEM
There was insufficient memory to create the kernel object.
```
https://linux.die.net/man/2/epoll_create1  

Ok. So let us peek into ```errno```.

```
errno - number of last error
```
https://linux.die.net/man/3/errno

Using libc, we can do this with:

```rust
#[derive(Debug)]
pub enum EpollErrors {
    CannotBeCreated(c_int),
}

impl Epoll {
    pub fn new() -> Result<Self, EpollErrors> {
        let fd = unsafe { libc::epoll_create1(1024) };
        if fd == -1 {
            let errno = unsafe { *libc::__errno_location() };
            Err(EpollErrors::CannotBeCreated(errno))
        } else {
            Ok(Self(fd))
        }
    }
}
```

Running now, we have:

```
> cargo run
thread 'main' panicked at 'called `Result::unwrap()` on an `Err` value: CannotBeCreated(22)', src/main.rs:29:30
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

Ok. We have error 22. What that means?
For that let us be a little bit more specific on the creation error enum.

```rust
#[derive(Debug)]
pub enum EpollCreationErrors {
    EINVAL,
    EMFILE,
    ENFILE,
    ENOMEM,
    UnknownError,
}
```

And our creation method looks like:

```rust
impl Epoll {
    pub fn new() -> Result<Self, EpollCreationErrors> {
        let fd = unsafe { libc::epoll_create1(1024) };
        if fd == -1 {
            let errno = unsafe { *libc::__errno_location() };
            let err = match errno {
                libc::EINVAL => EpollCreationErrors::EINVAL,
                libc::EMFILE => EpollCreationErrors::EMFILE,
                libc::ENFILE => EpollCreationErrors::ENFILE,
                libc::ENOMEM => EpollCreationErrors::ENOMEM,
                _ => EpollCreationErrors::UnknownError,
            };
            Err(err)
        } else {
            Ok(Self(fd))
        }
    }
}
```

If we run now

```
> cargo run 
thread 'main' panicked at 'called `Result::unwrap()` on an `Err` value: EINVAL', src/main.rs:40:30
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```

Ok. A little bit better. Let us read again what ```EINVAL``` means.

```
EINVAL
size is not positive.
EINVAL
(epoll_create1()) Invalid value specified in flags.
```
https://linux.die.net/man/2/epoll_create1

hum... ```epoll_create1``` does not have flags and ony has one paramter. Our value is not negative.
Documentation explicit asks it to be positive, but let us try zero.

```rust
impl Epoll {
    pub fn new() -> Result<Self, EpollCreationErrors> {
        let fd = unsafe { libc::epoll_create1(0) };
        ...
    }
}
```

Now let us run.

```
> cargo run
```

Hey. It worked. So my implementation here (WSL) do not support positive numbers as the documentation mandates.
Probably better to get used to this.

Let us move on.

We should also test if the ```epoll``` was closed correctly. There is not much we can do when dropping. But we can at least warn someone.
To be able to do that, we are going to need two more crates:

```toml
[dependencies]
libc = "0.2.126"
log = "0.4.17"
pretty_env_logger = "0.4.0"
```

and we need to initialize the logger on the very first line on ```main```.

```rust
fn main() {
    pretty_env_logger::init();
    let epoll = Epoll::new().unwrap();
}
```

Now we can check if the epoll was closed correctly and warn in case of failure.

```rust
impl Drop for Epoll {
    fn drop(&mut self) {
        let r = unsafe { libc::close(self.0) };
        if r != 0 {
            let errno = unsafe { *libc::__errno_location() };
            warn!("Failed when closing epoll {}. errno: {errno}", self.0);
        }
    }
}
```

Everything should be ok. We will first force this to error, but hardcoding a strange number.

```
let r = unsafe { libc::close(12345678) };
```

And when we run, we get:

```
> RUST_LOG=trace cargo run
WARN  async_runtime > Failed when closing epoll 3. errno: 9
```

That is nice. I think that is enough fir this. Let us return to our real value and test it.


```
> RUST_LOG=trace cargo run
```

Perfect.

### Ask Epoll to watch a file descriptor

Now that our ```epoll``` is running we need to ask it to watch for incoming messages on file descriptors. And we do this calling the function ```epoll_ctl```

```
int epoll_ctl(int epfd, int op, int fd, struct epoll_event *event);

It requests that the operation op be performed for the target file descriptor, fd.
Valid values for the op argument are :

EPOLL_CTL_ADD
Register the target file descriptor fd on the epoll instance 

The event argument describes the object linked to the file descriptor fd. The struct epoll_event is defined as :
typedef union epoll_data {
    void        *ptr;
    int          fd;
    uint32_t     u32;
    uint64_t     u64;
} epoll_data_t;

struct epoll_event {
    uint32_t     events;      /* Epoll events */
    epoll_data_t data;        /* User data variable */
};
```
https://linux.die.net/man/2/epoll_ctl

To call this function we need to create this ```epoll_event``` struct, which contains the ```events``` field, which is a bitset of flags, and some data, that can be anything. It is completely opaque to the ```epoll``` what this means. It just carry this and give it back to you later.

Ok. So we have:

```rust
fn main() {
    pretty_env_logger::init();
    let epoll = Epoll::new().unwrap();

    let mut e = libc::epoll_event {
        events: libc::EPOLLIN as u32,
        u64: 0,
    };
    unsafe { libc::epoll_ctl(epoll.0, libc::EPOLL_CTL_ADD, 0, &mut e) };
}
```

All this new code is doing is instructing our ```epoll``` that file descriptor 0 (the third parameter) can be read, and when it is ready to be ready, please return me the "u64" value I passed to you on the ```e``` variable.

We can run this and:

```
>  RUST_LOG=trace cargo run
```

It works. Great. 
But you know the gist now. We need to move this into the ```Epoll``` struct and check the return code.

So our ```main``` code becomes:

```rust
fn main() {
    pretty_env_logger::init();
    let mut epoll = Epoll::new().unwrap();
    epoll.add(0, 0).unwrap();
}
``` 

And our ```Epoll``` becomes:

```rust
#[derive(Debug)]
pub enum EpollAddErrors {
    EBADF,
    EEXIST,
    EINVAL,
    ENOENT,
    ENOMEM,
    ENOSPC,
    EPERM,
    UnknownError,
}

impl Epoll {
    ...

    pub fn add(&mut self, fd: usize, tag: u64) -> Result<(), EpollAddErrors> {
        let mut e = libc::epoll_event {
            events: libc::EPOLLIN as u32,
            u64: 0,
        };
        let r = unsafe { libc::epoll_ctl(self.0, libc::EPOLL_CTL_ADD, 0, &mut e) };
        if r == -1 {
            let errno = unsafe { *libc::__errno_location() };
            let err = match errno {
                libc::EBADF => EpollAddErrors::EBADF,
                libc::EEXIST => EpollAddErrors::EEXIST,
                libc::EINVAL => EpollAddErrors::EINVAL,
                libc::ENOENT => EpollAddErrors::ENOENT,
                libc::ENOMEM => EpollAddErrors::ENOMEM,
                libc::ENOSPC => EpollAddErrors::ENOSPC,
                libc::EPERM => EpollAddErrors::EPERM,
                _ => EpollAddErrors::UnknownError,
            };
            Err(err)
        } else {
            Ok(())
        }
    }
}
```

We can run, and everything works:

```
> RUST_LOG=trace cargo run
```

But what exactly we did here? We asked our ```epoll``` to watch for the file descriptior 0, which is the std input.
Can we do this? Well, yes we can!

Now we need to poll the ```epoll``` and do something when it signals me that I can read my file descriptor.

### Poll Epoll and wait to read

The ```epoll``` does not do anything on its own. What we need to do, is poll it and wait for anything to be in a readable state.
Remember that we asked it to wait notify us when stdin is ready to be read.

We do this calling ```epoll_wait```;

```
int epoll_wait(int epfd, struct epoll_event *events, int maxevents, int timeout);
```
https://linux.die.net/man/2/epoll_wait

The only complicated part here, is the ```events``` pointer. This is an area of memory where ```epoll``` will write and give us back the same ```epoll_event``` we gave it above when we added the file descriptor to its watching list.

We could do

```rust
let mut events: [libc::epoll_event; 1024];
```

But ```rustc``` would reming us that we would be potentially reading from uninitialized memory. Dangerous!

```
error[E0381]: borrow of possibly-uninitialized variable: `events`
  --> src/main.rs:90:62
   |
90 |         let event_count = unsafe { libc::epoll_wait(epoll.0, events.as_mut_ptr(), 1024, 30000) };
   |                                                              ^^^^^^^^^^^^^^^^^^^ use of possibly-uninitialized `events`
```

The point is that in this particular case, it is safe. We know that ```epoll_wait``` will not read this memory buffer, only write it.
To signal this to ```rustc```, we can do.

```rust
fn main() {
    pretty_env_logger::init();
    let mut epoll = Epoll::new().unwrap();
    epoll.add(0, 0).unwrap();

    let mut events: [libc::epoll_event; 1024] =
        unsafe { std::mem::MaybeUninit::zeroed().assume_init() };
    loop {
        let event_count = unsafe { libc::epoll_wait(epoll.0, events.as_mut_ptr(), events.len() as i32, 30000) };
        println!("Events: {event_count}");
    }
}
```

If we run this now, we see:

```
RUST_LOG=trace cargo run

```

We see the cursor blinking and the application never finishes. That happens for two reasons:
1 - ```epoll_wait``` blocks your current thread for the ammount of time you asked on the ```timeout``` parameter above. In our case 30000 milliseconds;
2 - We have, of course, a loop around this call.

The interesting part happens if we actually send something to the std input.
We can type anything in out console and hit <Enter>.

If you do this, you will see:

```
> RUST_LOG=trace cargo run
Events: 1
Events: 1
Events: 1
Events: 1
Events: 1
Events: 1
...
```

And so on... until you hit <Control+C> to break the application.
That happens because we are following this loop:

1 - Hey, ```epoll``` is any of the file descriptors in your watchlist, ready to be read?
2 - Yes! I have one file descriptor ready. Look at the list you passed me.
3 - Nice. Let me print how many descriptors are ready;
4 - Hey, ```epoll``` is any of the file descriptors in your watchlist, ready to be read?
5 - Yes! I have one file descriptor ready. Look at the list you passed me.

and so on.... You got the idea.
We never read the descriptor, which means it is still ready.

There are two things we can do. First and the easiest is to read from the std in.

And we can very easily do this with:

```rust
let mut events: [libc::epoll_event; 1024] =
    unsafe { std::mem::MaybeUninit::zeroed().assume_init() };
loop {
    let event_count = unsafe { libc::epoll_wait(epoll.0, events.as_mut_ptr(), events.len() as i32, 30000) };
    println!("Events: {event_count}");
    let mut buffer = [0u8; 1024];
    for e in &events[0..event_count as usize] {
        loop {
            let len = unsafe {
                libc::read(
                    e.u64 as i32,
                    buffer.as_mut_ptr() as *mut std::ffi::c_void,
                    buffer.len(),
                )
            };
            println!("Length: {len}");
            if len <= 0 {
                println!("Done.");
                break;
            }
        }
    }
}
```

When ```epoll_wait``` returns, it returns how many descriptors are ready.
This means we can iterator in the array until this number and do something which each event.

For now, we now that the event "tag" zero, means that the std input is ready. We don't know what to do with others values yet.
So in this case, we read everything the stdinput has, until it returns zero.

If we run now, we will see our application waiting for std input.
We can type something, hit enter so the terminal actually writes to the std input.
And the application enter in loop again.

```
> RUST_LOG=trace cargo run
a
Events: 1

```

This is great! What is not obvious is that if we type something again and hit <Enter>, we do not see the "Events: 1" again.

What is happening?
All of this for nothing?

To understand what is happening. Let us put some ```println```s to see where the code is blocking.

```rust
loop {
    let event_count =
        unsafe { libc::epoll_wait(epoll.0, events.as_mut_ptr(), events.len() as i32, 30000) };
    println!("Events: {event_count}");
    for e in &events[0..event_count as usize] {
        let mut buffer = [0u8; 1024];
        loop {
            let len = unsafe {
                libc::read(
                    e.u64 as i32,
                    buffer.as_mut_ptr() as *mut std::ffi::c_void,
                    buffer.len(),
                )
            };
            println!("Length: {len}");
            if len <= 0 {
                println!("Done.");
                break;
            }
        }
    }
}
```

And if we run now, we have:

```
> RUST_LOG=trace cargo run
a
Events: 1
Length: 2
```

So we printed the length. But we never printed "Done".
Which means that our call is blocking on ```read```.

Pase for a second. We did all this to block on ```read```? Does that makes sense?
Off course it does not!

We are blocking here, because we are missing on final step.
We need to ask the OS to make that particular file descriptor "non-blocking".
This means that when we read it, instead of blicking waiting for the input, it returns immeadiately telling me: "Hey, I have nothing to read and I would block".

So let us do this:

```rust
fn main() {
    pretty_env_logger::init();

    let flags = unsafe { libc::fcntl(0, libc::F_GETFL, 0) };
    unsafe { libc::fcntl(0, libc::F_SETFL, flags | libc::O_NONBLOCK) };
    ...
}
```

We don't need to go too deep here for the moment. Just understand that we get the current set of flags and add a new one: ```O_NONBLOCK```.
Now if we run our server again:

```
> RUST_LOG=trace cargo run
a
Events: 1
Length: 2
Length: -1
Done.
b
Events: 1
Length: 2
Length: -1
Done.
c
Events: 1
Length: 2
Length: -1
Done.
```

Oh! Beautiful! We are correctly consuming std input using epoll now.

### Refactor this mess

Now we are going to take our first step into ```async``` world.
Instead of immediatly cleaning up the mess we are, we are just going to remove the ```printls```.

The second thing we want to do, is simplify our loop.
We dont want to have a huge if/match there for all possible file descriptors we have.

For this we are going to create a ```HashMap``` containing every single piece of code waiting for descriptors to be ready: let us call this ```sleeping```.

```rust
fn main() {
    pretty_env_logger::init();

    let mut sleeping = HashMap::new();
    ...
}
```

When we register the std input into the ```epoll```, let us also insert a sleeping callback.
```
fn main() {
    ...
    let mut epoll = Epoll::new().unwrap();
    epoll.add(0, 0).unwrap();
    sleeping.insert(0, || {
        println!("fd 0 is ready");
        let mut buffer = [0u8; 1024];
        loop {
            let len = unsafe {
                libc::read(
                    0,
                    buffer.as_mut_ptr() as *mut std::ffi::c_void,
                    buffer.len(),
                )
            };
            if len <= 0 {
                break;
            }
        }
    });    
}
```

This means that this closure is sleeping waiting for file descriptor zero to be ready. Nice!
Let us also simplify our loop to:

```rust
let mut events: [libc::epoll_event; 1024] =
    unsafe { std::mem::MaybeUninit::zeroed().assume_init() };
loop {
    let event_count =
        unsafe { libc::epoll_wait(epoll.0, events.as_mut_ptr(), events.len() as i32, 30000) };
    for e in &events[0..event_count as usize] {
        if let Some(wake) = sleeping.get(&e.u64) {
            wake();
        }
    }
}
```

If we run now we have:

```
> RUST_LOG=trace cargo run
a
fd 0 is ready
b
fd 0 is ready
c
fd 0 is ready
```

That is nice. We can even move the ```sleeping``` HashMap to our ```Epoll``` struct.

```rust
pub struct Epoll {
    fd: c_int,
    sleeping: HashMap<u64, Box<dyn Fn()>>,
}
```

Giving that we could potentially store any closure, we need to box them and ```dyn Fn()``` will work for now.
Our ```add``` function can receive this closure now. And we can insert it into the hash map, if everything is ok.

```rust
impl Epoll {
    ...

    pub fn add(&mut self, fd: i32, tag: u64, f: impl Fn() + 'static) -> Result<(), EpollAddErrors> {
        let mut e = libc::epoll_event {
            events: libc::EPOLLIN as u32,
            u64: 0,
        };
        let r = unsafe { libc::epoll_ctl(self.fd, libc::EPOLL_CTL_ADD, 0, &mut e) };
        if r == -1 {
            ...
            Err(err)
        } else {
            self.sleeping.insert(fd as u64, Box::new(f));
            Ok(())
        }
    }

    ...
}
```

Last step is to move the ```epoll_wait``` to inside our ```Epoll```. We also changed the timeout to "-1" which means, "wait forever".

```rust
impl Epoll {
    ...

    pub fn wait(&self) {
        let mut events: [libc::epoll_event; 1024] =
            unsafe { std::mem::MaybeUninit::zeroed().assume_init() };
        let event_count =
            unsafe { libc::epoll_wait(self.fd, events.as_mut_ptr(), events.len() as i32, -1) };
        for e in &events[0..event_count as usize] {
            if let Some(wake) = self.sleeping.get(&e.u64) {
                wake();
            }
        }
    }
}
```

With this our "main" function became:

```rust
fn main() {
    pretty_env_logger::init();

    let flags = unsafe { libc::fcntl(0, libc::F_GETFL, 0) };
    unsafe { libc::fcntl(0, libc::F_SETFL, flags | libc::O_NONBLOCK) };

    let mut epoll = Epoll::new().unwrap();
    epoll.add(0, 0, || {
        println!("fd 0 is ready");
        let mut buffer = [0u8; 1024];
        loop {
            let len = unsafe {
                libc::read(
                    0,
                    buffer.as_mut_ptr() as *mut std::ffi::c_void,
                    buffer.len(),
                )
            };
            if len <= 0 {
                break;
            }
        }
    });

    loop {
        epoll.wait();
    }
}
```

Which can be simplified if we move the closure to a function.

```rust

fn read_stdin() {
    println!("fd 0 is ready");
    let mut buffer = [0u8; 1024];
    loop {
        let len = unsafe {
            libc::read(
                0,
                buffer.as_mut_ptr() as *mut std::ffi::c_void,
                buffer.len(),
            )
        };
        if len <= 0 {
            break;
        }
    }
}

fn main() {
    pretty_env_logger::init();

    let flags = unsafe { libc::fcntl(0, libc::F_GETFL, 0) };
    unsafe { libc::fcntl(0, libc::F_SETFL, flags | libc::O_NONBLOCK) };

    let mut epoll = Epoll::new().unwrap();
    epoll.add(0, 0, read_stdin);

    loop {
        epoll.wait();
    }
}
```

Ok. That is quite reasonable. If we run. Everything is working as expected.

```
> RUST_LOG=trace cargo run
a
fd 0 is ready
b
fd 0 is ready
c
fd 0 is ready
```

### Inverting the logic

Our solution is much better, but still has a couple of strange things.
First is that we loop and wait forever.


```rust
loop {
    epoll.wait();
}
```

The second is that we are storing the callback inside the epoll, and for that we need to box closures and store them as ```dyn Fn()```. 
And the ```epoll``` should not care about this at all.
It should only signal to us that something we were waiting is done.

And for that there is a better solution. We can use what OSes call "events".
Yeah. Another meaning for the same word.

What we are going to do now is:
1 - create the OS event;
2 - pass the event description to the ```Epoll``` class;
3 - when a file descriptor signal is ready, we signal this event instead of calling a callback;
4 - The "outside" code wait for this event.