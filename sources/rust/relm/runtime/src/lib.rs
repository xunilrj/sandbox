use std::{alloc::Layout, collections::VecDeque};

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
            MessageFactory::OnClick(_) => {
                write!(f, "OnClick");
            }
            MessageFactory::OnInput(_) => {
                write!(f, "OnInput");
            }
            _ => {}
        };
        Ok(())
    }
}

// pub type UpdateFunction<TMessage, TState> = fn(message: &TMessage, state: &mut TState);
pub type ViewResult<TMessage> = (Html, Vec<MessageFactory<TMessage>>);
// pub type ViewFunction<TState, TMessage: std::fmt::Debug> = fn(&TState) -> ViewResult<TMessage>;

pub trait ApplicationFacade {
    fn send_by_id(
        &mut self,
        id: usize,
        p: Vec<u64>,
        messages: *mut (),
    ) -> std::result::Result<String, u64>;
    fn render(&mut self, messages: *mut ()) -> Html;
}

pub struct ApplicationWrapper {
    pub facade: Box<dyn ApplicationFacade>,
    pub messages: *mut (),
}

pub trait ApplicationTrait {
    type State;
    type Message: Clone + std::fmt::Debug;
    type Actions: Clone + std::fmt::Debug;

    fn update(&mut self, message: &Self::Message, commands: &mut Vec<Self::Actions>);
    fn to_html(&self) -> ViewResult<Self::Message>;

    fn send(&mut self, message: &Self::Message) {
        let mut commands = Vec::new();
        self.update(message, &mut commands);
    }

    fn render2(&mut self, messages: *mut ()) -> Html {
        let v = unsafe { &mut *(messages as *mut Vec<MessageFactory<Self::Message>>) };
        v.clear();

        let (html, messages) = self.to_html();

        // println!("HTML: {:?}", html);
        // println!("Messages: {:?}", messages);

        v.extend(messages);

        unsafe {
            apply_to(html.len() as u32, html.as_ptr() as u32);
        }

        html
    }

    fn send_by_id2(
        &mut self,
        id: usize,
        p: Vec<u64>,
        messages: *mut (),
    ) -> std::result::Result<String, u64> {
        let messages = unsafe { &mut *(messages as *mut Vec<MessageFactory<Self::Message>>) };
        let message_factory = messages.get_mut(id).ok_or(0u64)?;

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
            self.send(&msg);
            let html = self.render2(messages as *mut Vec<MessageFactory<Self::Message>> as *mut ());
            Ok(html)
        } else {
            Err(0)
        }
    }
}

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

impl std::default::Default for InputState {
    fn default() -> Self {
        Self {
            value: "".to_string(),
        }
    }
}

// pub static mut APPS: Vec<ApplicationWrapper> = Vec::new();

pub static mut APPS2: Vec<Application> = Vec::new();

pub struct Application {
    apps: Vec<ApplicationWrapper>,
}

impl Application {
    pub fn new() -> Self {
        Self { apps: Vec::new() }
    }

    pub fn mount(&mut self, wrapper: ApplicationWrapper) -> usize {
        self.apps.push(wrapper);
        self.apps.len() - 1
    }

    pub fn send(&mut self, app_id: usize, msg_id: usize, p: Vec<u64>) -> bool {
        match self.apps.get_mut(app_id) {
            Some(ApplicationWrapper { facade, messages }) => {
                match facade.send_by_id(msg_id, p, *messages) {
                    Ok(_) => true,
                    Err(_) => false,
                }
            }
            None => false,
        }
    }

    pub fn render(&mut self, id: usize) {
        match self.apps.get_mut(id) {
            Some(ApplicationWrapper { facade, messages }) => {
                facade.render(*messages);
            }
            None => {}
        }
    }
}

#[no_mangle]
pub fn init() {
    std::panic::set_hook(Box::new(|panic_info| {
        let msg = format!("{}", panic_info);
        unsafe { console_error(msg.len() as u32, msg.as_ptr() as u32) };
    }));
}

#[no_mangle]
pub fn application_new() -> usize {
    let app = Application::new();
    unsafe {
        APPS2.push(app);
        APPS2.len() - 1
    }
}

#[macro_export]
macro_rules! mount {
    ( $($id:expr => $type:ty),*) => {

        $(impl runtime::ApplicationFacade for $type {
            fn send_by_id(&mut self, id: usize, p: Vec<u64>, messages: *mut ()) -> Result<String, u64> { self.send_by_id2(id, p, messages) }
            fn render(&mut self, messages: *mut ()) -> Html { self.render2(messages) }
        })*

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
                                        Box::new(Vec::<MessageFactory<<$type as ApplicationTrait>::Message>>::new())
                                    ) as *mut Vec::<MessageFactory<<$type as ApplicationTrait>::Message>> as *mut ()
                                )),
                            )*
                            _ => None,
                        };

                        match r {
                            Some((facade, messages)) => unsafe {
                                let id = app.mount(ApplicationWrapper { facade, messages });
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
    app_id: usize,
    wrapper_id: usize,
    msg_idx: usize,
    p0: u64,
    p1: u64,
    p2: u64,
    p3: u64,
    p4: u64,
) -> bool {
    let p = vec![p0, p1, p2, p3, p4];
    unsafe {
        match APPS2.get_mut(app_id as usize) {
            Some(app) => app.send(wrapper_id as usize, msg_idx as usize, p),
            None => false,
        }
    }
}

#[cfg(target_arch = "wasm32")]
extern "C" {
    pub fn apply_to(size: u32, ptr: u32);
    pub fn console_error(size: u32, ptr: u32);
}

#[cfg(target_arch = "x86_64")]
pub fn apply_to(size: u32, ptr: u32) {}

#[cfg(target_arch = "x86_64")]
pub fn console_error(size: u32, ptr: u32) {}

#[track_caller]
pub fn console_error_str(s: &str) {
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

fn alloc_str(s: &str) -> AllocBuffer {
    let mut buffer = AllocBuffer::new(s.len());
    buffer.copy_str(s).unwrap();
    buffer
}

#[cfg(target_arch = "x86_64")]
fn pretty_print_node<'a>(
    depth: usize,
    r: &'a ego_tree::NodeRef<'a, scraper::Node>,
    newhtml: &mut String,
) {
    use std::fmt::Write;

    let node = r.value();
    let spaces = " ".repeat(depth * 2);
    if let Some(element) = node.as_element() {
        write!(newhtml, "{}<{}", spaces, element.name.local);

        let mut attrs: Vec<_> = element.attrs().collect();
        attrs.sort_by(|(lk, _), (rk, _)| lk.cmp(rk));
        for (k, v) in attrs.iter() {
            write!(newhtml, " {}=\"{}\"", k, v);
        }

        writeln!(newhtml, ">");
    }
    if let Some(txt) = node.as_text() {
        writeln!(newhtml, "{}{}", spaces, txt.to_string());
    }

    for child in r.children() {
        pretty_print_node(depth + 1, &child, newhtml);
    }

    if let Some(element) = node.as_element() {
        writeln!(newhtml, "{}</{}>", spaces, element.name.local);
    }
}

#[cfg(target_arch = "x86_64")]
fn pretty_print_html(doc: &scraper::Html, newhtml: &mut String) {
    let root = doc.tree.root();
    for child in root.children() {
        for child in child.children() {
            pretty_print_node(0, &child, newhtml);
        }
    }
}

#[cfg(target_arch = "x86_64")]
fn parse_and_print(html: &str) -> (scraper::Html, String) {
    let document = scraper::Html::parse_fragment(html);

    let mut newhtml = String::new();
    pretty_print_html(&document, &mut newhtml);

    (document, newhtml)
}

#[cfg(target_arch = "x86_64")]
pub struct Tester<T: Default + ApplicationTrait> {
    state: T,
    messages: Vec<MessageFactory<<T as ApplicationTrait>::Message>>,
    document: scraper::Html,
    pretty_html: String,
}

#[cfg(target_arch = "x86_64")]
impl<T: Default + ApplicationTrait> Tester<T> {
    pub fn new() -> Self {
        let state: T = Default::default();
        let (html, messages) = state.to_html();

        let (document, pretty_html) = parse_and_print(html.as_str());

        Self {
            state,
            messages,
            document,
            pretty_html,
        }
    }

    fn get_message_id(&self, selector: &str, idx: usize, event_name: &str) -> Option<usize> {
        let selector = scraper::Selector::parse(selector).unwrap();
        let mut s = self.document.select(&selector);
        let el = s.nth(idx).unwrap();
        let el = el.value();

        if let Some(s) = el.attr(event_name) {
            let i = s.find("send(").unwrap();
            let (_, s) = s.split_at(i);
            if s.starts_with("send(") {
                let mut parameters = s
                    .trim_start_matches("send(")
                    .trim_end_matches(");")
                    .split(",");
                Some(parameters.nth(0).unwrap().parse::<u64>().unwrap() as usize)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn click(&mut self, query: &str, idx: usize) {
        if let Some(msg_id) = self.get_message_id(query, idx, "onclick") {
            println!("Clicking {:?}", query);
            if let Ok(html) = self.state.send_by_id2(
                msg_id,
                Vec::new(),
                &mut self.messages as *mut Vec<MessageFactory<<T as ApplicationTrait>::Message>>
                    as *mut (),
            ) {
                let (document, new_pretty_html) = parse_and_print(html.as_str());

                let diff =
                    prettydiff::diff_lines(self.pretty_html.as_str(), new_pretty_html.as_str());

                println!("----------------------------------");
                println!("{}", diff);
                println!("----------------------------------");

                self.document = document;
                self.pretty_html = new_pretty_html;
            } else {
                panic!("element not found");
            }
        } else {
            panic!("element not found");
        }
    }

    pub fn input(&mut self, query: &str, idx: usize, text: &str) {
        if let Some(msg_id) = self.get_message_id(query, idx, "oninput") {
            let messages = &mut self.messages
                as *mut Vec<MessageFactory<<T as ApplicationTrait>::Message>>
                as *mut ();
            let addr = alloc_str(text);
            let ps = vec![addr.data_ptr() as u64];
            std::mem::forget(addr);
            println!("Sending input {:?} to {:?}", text, query);
            if let Ok(html) = self.state.send_by_id2(msg_id, ps, messages) {
                let (document, new_pretty_html) = parse_and_print(html.as_str());

                let diff =
                    prettydiff::diff_lines(self.pretty_html.as_str(), new_pretty_html.as_str());
                println!("----------------------------------");
                println!("{}", diff);
                println!("----------------------------------");

                self.document = document;
                self.pretty_html = new_pretty_html;
            } else {
                panic!("element not found");
            }
        } else {
            panic!("element not found");
        }
    }

    pub fn assert_inner_text<F>(&mut self, query: &str, f: F)
    where
        F: Fn(&str) -> bool,
    {
        let selector = scraper::Selector::parse(query).unwrap();
        let mut s = self.document.select(&selector);
        if let Some(node) = s.nth(0) {
            let inner_text: Vec<_> = node.text().collect();
            let inner_text = format!("{}", inner_text.iter().nth(0).unwrap());
            let r = f(inner_text.as_str());
            if !r {
                println!("Failed: {}", inner_text);
            }
            assert!(r);
        } else {
            assert!(false);
        }
    }
}
