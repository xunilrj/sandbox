use std::{alloc::Layout, str::FromStr, sync::RwLock};

#[derive(Debug)]
pub struct OnInput {
    pub data: String,
}
#[derive(Debug)]
pub struct OnClick {}

pub enum MessageFactory<TMessage: std::fmt::Debug> {
    Message(TMessage),
    OnInput(Box<dyn Fn(OnInput) -> TMessage>),
    OnClick(Box<dyn Fn(OnClick) -> TMessage>),
}

impl<TMessage: std::fmt::Debug> std::fmt::Debug for MessageFactory<TMessage> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            MessageFactory::Message(msg) => {
                write!(f, "{:?}", msg);
            }
            _ => {}
        };
        Ok(())
    }
}

pub type InitFunction<T> = fn() -> T;
pub type UpdateFunction<TMessage, TState> = fn(message: &TMessage, state: &mut TState);
pub type ViewResult<TMessage> = (Html, Vec<MessageFactory<TMessage>>);
pub type ViewFunction<TState, TMessage: std::fmt::Debug> = fn(&TState) -> ViewResult<TMessage>;

pub type Html = String;

pub struct InputState {
    pub value: String,
}

impl InputState {
    pub fn new() -> Self {
        Self {
            value: "".to_string(),
        }
    }

    pub fn parse<F: std::str::FromStr>(&self) -> Result<F, F::Err> {
        self.value.parse::<F>()
    }
}

impl From<&str> for InputState {
    fn from(s: &str) -> Self {
        Self {
            value: s.to_string(),
        }
    }
}

pub struct Application<TState, TMessage: std::fmt::Debug + Clone> {
    state: TState,
    update: UpdateFunction<TMessage, TState>,
    view: ViewFunction<TState, TMessage>,
    messages: Vec<MessageFactory<TMessage>>,
}

impl<TState, TMessage: std::fmt::Debug + Clone> Application<TState, TMessage> {
    pub fn new(
        init: InitFunction<TState>,
        update: UpdateFunction<TMessage, TState>,
        view: ViewFunction<TState, TMessage>,
    ) -> Self {
        Self {
            state: init(),
            update,
            view,
            messages: Vec::new(),
        }
    }

    pub fn send(&mut self, message: &TMessage) {
        let f = self.update;
        f(message, &mut self.state);
    }

    pub fn send_by_id(&mut self, id: usize, p: Vec<u64>) -> std::result::Result<(), u64> {
        let message_factory = self.messages.get_mut(id).ok_or(0u64)?;

        let msg = match message_factory {
            MessageFactory::Message(msg) => Some(msg.clone()),
            MessageFactory::OnClick(e) => {
                let onclick = OnClick {};
                let f = e.as_ref();
                Some(f(onclick))
            }
            MessageFactory::OnInput(e) => {
                let p0 = p.get(0).ok_or(0u64)?;
                let buffer = AllocBuffer::from_data_ptr(*p0 as usize);
                let data = buffer.as_str().map_err(|_| 0u64)?.to_owned();

                let onclick = OnInput {
                    data: data.to_string(),
                };
                let f = e.as_ref();
                Some(f(onclick))
            }
        };

        if let Some(msg) = msg {
            let f = self.update;
            f(&msg, &mut self.state);
            self.render();
        }

        Ok(())
    }

    pub fn render(&mut self) -> Html {
        self.messages.clear();

        let f = self.view;
        let (html, messages) = f(&self.state);

        println!("HTML: {:?}", html);
        println!("Messages: {:?}", messages);

        self.messages.extend(messages);

        unsafe {
            apply_to(html.len() as u32, html.as_ptr() as u32);
        }

        html
    }
}

#[cfg(target_arch = "wasm32")]
extern "C" {
    pub fn apply_to(size: u32, ptr: u32);
    pub fn console_error(size: u32, ptr: u32);
}

#[cfg(unix)]
pub fn apply_to(size: u32, ptr: u32) {}

#[cfg(unix)]
pub fn console_error(size: u32, ptr: u32) {}

#[track_caller]
fn console_error_str(s: &str) {
    let location = core::panic::Location::caller();
    let s = format!("{} at {}", s, location);
    unsafe { console_error(s.len() as u32, s.as_ptr() as u32) };
}

struct AllocBuffer {
    ptr: *mut u8,
}

impl AllocBuffer {
    pub fn header_size() -> usize {
        std::mem::size_of::<usize>()
    }

    pub fn from_data_ptr(ptr: usize) -> Self {
        let ptr_start = ptr - Self::header_size();
        let block = Self {
            ptr: ptr_start as *mut u8,
        };

        block
    }

    pub fn new(size: usize) -> Self {
        let total_size = Self::header_size() + size as usize;

        let layout = Layout::array::<u8>(total_size).unwrap();
        let ptr = unsafe { std::alloc::alloc(layout) };

        let block_size = ptr as *mut usize;
        unsafe { *block_size = total_size };

        Self { ptr }
    }

    pub fn total_size(&self) -> usize {
        let total_size = self.ptr as *mut usize;
        unsafe { *total_size }
    }

    pub fn data_size(&self) -> usize {
        self.total_size() - Self::header_size()
    }

    pub fn data_ptr(&self) -> *mut u8 {
        let ptr = self.ptr as usize;
        (ptr + Self::header_size()) as *mut u8
    }

    pub fn as_slice_mut(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.data_ptr(), self.data_size()) }
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data_ptr(), self.data_size()) }
    }

    pub fn as_str(&self) -> Result<&str, std::str::Utf8Error> {
        let slice = self.as_slice();
        std::str::from_utf8(slice)
    }
}

impl Drop for AllocBuffer {
    fn drop(&mut self) {
        let layout = Layout::array::<u8>(self.total_size()).unwrap();
        unsafe { std::alloc::dealloc(self.ptr, layout) };
    }
}

#[no_mangle]
pub fn alloc(size: u64) -> u64 {
    let buffer = AllocBuffer::new(size as usize);
    let ptr = buffer.data_ptr() as u64;
    std::mem::forget(buffer);
    ptr
}

// #[no_mangle]
// pub fn mount() -> u64 {
//     std::panic::set_hook(Box::new(|panic_info| {
//         let msg = format!("{}", panic_info);
//         unsafe { console_error(msg.len() as u32, msg.as_ptr() as u32) };
//     }));

//     let app = login::App::new(login::init, login::update, login::view);

//     unsafe {
//         APPS.push(Applications::Login(app));
//         let id = APPS.len() as u64 - 1;

//         match APPS.get_mut(id as usize) {
//             Some(app) => {
//                 app.render();
//             }
//             None => {}
//         }

//         id
//     }
// }
