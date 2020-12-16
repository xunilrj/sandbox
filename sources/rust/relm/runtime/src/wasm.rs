use crate::*;
use std::alloc::Layout;

pub static mut APPS2: Vec<Application> = Vec::new();
static LOGGER: WebConsoleLogger = WebConsoleLogger {};

struct WebConsoleLogger {}

impl log::Log for WebConsoleLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::max_level()
    }

    fn log(&self, record: &log::Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        let txt = &format!(
            "{} [{}] @ {}:{}",
            record.target(),
            record.args(),
            record.file().unwrap(),
            record.line().unwrap()
        );

        unsafe {
            match record.level() {
                log::Level::Error => console(0, txt.len() as u32, txt.as_ptr() as u32),
                log::Level::Warn => console(1, txt.len() as u32, txt.as_ptr() as u32),
                log::Level::Info => console(2, txt.len() as u32, txt.as_ptr() as u32),
                log::Level::Debug => console(3, txt.len() as u32, txt.as_ptr() as u32),
                log::Level::Trace => console(4, txt.len() as u32, txt.as_ptr() as u32),
            }
        }
    }

    fn flush(&self) {}
}

#[cfg(target_arch = "wasm32")]
extern "C" {
    pub fn apply_to(size: u32, ptr: u32);
    pub fn console(category: u32, size: u32, ptr: u32);
}

#[cfg(target_arch = "x86_64")]
pub unsafe fn apply_to(_size: u32, _ptr: u32) {}

#[cfg(target_arch = "x86_64")]
pub unsafe fn console(_category: u32, _size: u32, _ptr: u32) {}

#[no_mangle]
pub fn init() {
    std::panic::set_hook(Box::new(|panic_info| {
        let msg = format!("{}", panic_info);
        unsafe { console(0, msg.len() as u32, msg.as_ptr() as u32) };
    }));

    let _ = log::set_logger(&LOGGER);
    log::set_max_level(log::LevelFilter::Trace);
}

#[no_mangle]
pub fn application_new() -> usize {
    unsafe {
        let app = Application::new(APPS2.len());
        APPS2.push(app);
        APPS2.len() - 1
    }
}

#[macro_export]
macro_rules! mount {
    ( $($id:expr => $type:ty),*) => {
        $(impl runtime::ApplicationFacade for $type {
            fn send_by_id(&mut self,
                app: usize,
                actor: usize,
                id: usize,
                p: Vec<u64>,
                messages: *mut (),
                executor: &mut runtime::executor::Executor) -> Result<(), u64>
            {
                match self.build_message(id, p, messages) {
                    Ok(message) => {
                        #[cfg(feature = "derive_debug")]
                        {
                            log::trace!(target: "actor", "{}:{} Update: [{:?}]", app, actor, message);
                        }
                        let mut actions = self.send(&message);

                        while let Some(action) = actions.pop() {
                            #[cfg(feature = "derive_debug")]
                            {
                                log::trace!(target: "actor", "{}:{} Action: [{:?}]", app, actor, action);
                            }
                            let env = env::Env::new(app, actor);
                            let handle = action.handle(env);
                            executor.spawn(handle);
                        }

                        Ok(())
                    }
                    _ => Err(0)
                }
            }

            fn render(&mut self, app: usize, actor: usize, messages: *mut ()) -> Html {
                <$type as DisplayHtml>::render2(self, app, actor, messages)
            }
        }

        )*

        #[no_mangle]
        pub fn application_mount(app_id: usize, type_id: usize) -> isize {
            unsafe {
                match APPS2.get_mut(app_id as usize) {
                    Some(app) => {
                        let r = match type_id {
                            $(
                                $id => Some((
                                    Box::new(<$type as std::default::Default>::default()) as Box<dyn ApplicationFacade>,
                                    Box::leak(
                                        Box::new(Vec::<MessageFactory<<$type as UpdatableState>::Message>>::new())
                                    ) as *mut Vec::<MessageFactory<<$type as UpdatableState>::Message>> as *mut ()
                                )),
                            )*
                            _ => None,
                        };

                        match r {
                            Some((facade, messages)) => unsafe {
                                let id = app.mount(Actor { facade, messages });
                                app.render(id);
                                id as isize
                            },
                            None => -1,
                        }
                    },
                    None => -1
                }
            }
        }
    };
}

#[no_mangle]
pub fn application_send(
    app: usize,
    actor: usize,
    msg: usize,
    p0: u64,
    p1: u64,
    p2: u64,
    p3: u64,
    p4: u64,
) -> bool {
    let p = vec![p0, p1, p2, p3, p4];
    unsafe {
        match APPS2.get_mut(app as usize) {
            Some(app) => {
                app.send(actor as usize, msg as usize, p);
                app.render(actor);
                true
            }
            None => false,
        }
    }
}

pub fn application_get_messages(app_id: usize, wrapper_id: usize) -> Option<*mut ()> {
    unsafe {
        match APPS2
            .get_mut(app_id as usize)
            .and_then(|x| x.actors.get(wrapper_id))
        {
            Some(actor) => Some(actor.messages),
            None => None,
        }
    }
}

#[track_caller]
pub fn console_error_str(s: &str) {
    let location = core::panic::Location::caller();
    let s = format!("{} at {}", s, location);
    unsafe { console(0, s.len() as u32, s.as_ptr() as u32) };
}

pub struct AllocBuffer {
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

    pub fn copy_str(&mut self, s: &str) -> Result<(), u64> {
        if s.len() > self.data_size() {
            Err(0)
        } else {
            let data = self.as_slice_mut();
            data.copy_from_slice(s.as_bytes());
            Ok(())
        }
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

pub fn alloc_str(s: &str) -> AllocBuffer {
    let mut buffer = AllocBuffer::new(s.len());
    buffer.copy_str(s).unwrap();
    buffer
}
