pub mod env;
pub mod executor;
pub mod tester;
pub mod wasm;
use executor::Executor;
pub use wasm::*;

#[cfg(not(feature = "derive_debug"))]
pub trait MyDebug {}
#[cfg(not(feature = "derive_debug"))]
impl<T> MyDebug for T {}

#[cfg(feature = "derive_debug")]
pub trait MyDebug: std::fmt::Debug {}

#[cfg(feature = "derive_debug")]
impl<T: std::fmt::Debug> MyDebug for T {}

#[cfg_attr(feature = "derive_debug", derive(Debug))]
pub struct OnInput {
    pub data: String,
}
#[cfg_attr(feature = "derive_debug", derive(Debug))]
pub struct OnClick {}

pub enum HtmlEvents {
    OnInput(OnInput),
    OnClick(OnClick),
}

impl From<OnInput> for HtmlEvents {
    fn from(v: OnInput) -> Self {
        HtmlEvents::OnInput(v)
    }
}

impl From<OnClick> for HtmlEvents {
    fn from(v: OnClick) -> Self {
        HtmlEvents::OnClick(v)
    }
}

pub enum MessageFactory<TMessage: MyDebug> {
    Message(TMessage),
    OnInput(Box<dyn Fn(OnInput) -> TMessage>),
    OnClick(Box<dyn Fn(OnClick) -> TMessage>),
}

#[cfg(feature = "derive-debug")]
impl<TMessage: MyDebug> std::fmt::Debug for MessageFactory<TMessage> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            MessageFactory::Message(msg) => {
                write!(f, "{:?}", msg).unwrap();
            }
            MessageFactory::OnClick(_) => {
                write!(f, "OnClick").unwrap();
            }
            MessageFactory::OnInput(_) => {
                write!(f, "OnInput").unwrap();
            }
        };
        Ok(())
    }
}

pub trait ApplicationFacade {
    fn send_by_id(
        &mut self,
        app: usize,
        actor: usize,
        msg: usize,
        p: Vec<u64>,
        messages: *mut (),
        executor: &mut Executor,
    ) -> std::result::Result<(), u64>;
    fn render(&mut self, app: usize, actor: usize, messages: *mut ()) -> Html;
}

pub struct Actor {
    pub facade: Box<dyn ApplicationFacade>,
    pub messages: *mut (),
}

pub trait UpdatableState {
    type State;
    type Message: Clone + MyDebug;
    type Actions: Clone + MyDebug;

    fn update(&mut self, message: &Self::Message, commands: &mut Vec<Self::Actions>);

    fn send(&mut self, message: &Self::Message) -> Vec<Self::Actions> {
        let mut commands = Vec::new();
        self.update(message, &mut commands);

        commands
    }

    fn build_message(
        &mut self,
        id: usize,
        p: Vec<u64>,
        messages: *mut (),
    ) -> std::result::Result<Self::Message, u64> {
        let messages = unsafe { &mut *(messages as *mut Vec<MessageFactory<Self::Message>>) };
        let message_factory = messages.get_mut(id).ok_or(0u64)?;
        match message_factory {
            MessageFactory::Message(msg) => Ok(msg.clone()),
            MessageFactory::OnClick(e) => {
                let onclick = OnClick {};
                let f = e.as_ref();
                Ok(f(onclick))
            }
            MessageFactory::OnInput(e) => {
                let p0 = p.get(0).ok_or(0u64)?;
                let buffer = AllocBuffer::from_data_ptr(*p0 as usize);
                let data = buffer.as_str().map_err(|_| 0u64)?.to_owned();

                let onclick = OnInput {
                    data: data.to_string(),
                };
                let f = e.as_ref();
                Ok(f(onclick))
            }
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

    pub fn clear(&mut self) {
        self.value = "".to_string();
    }

    pub fn is_empty(&mut self) -> bool {
        self.value.is_empty()
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

pub struct Application {
    id: usize,
    actors: Vec<Actor>,
    executor: Executor,
}

impl Application {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            actors: Vec::new(),
            executor: Executor::new(),
        }
    }

    pub fn mount(&mut self, actor: Actor) -> usize {
        self.actors.push(actor);
        self.actors.len() - 1
    }

    pub fn send(&mut self, actor: usize, msg: usize, message_parameters: Vec<u64>) -> bool {
        match self.actors.get_mut(actor) {
            Some(Actor { facade, messages }) => {
                match facade.send_by_id(
                    self.id,
                    actor,
                    msg,
                    message_parameters,
                    *messages,
                    &mut self.executor,
                ) {
                    Ok(_) => true,
                    Err(_) => false,
                }
            }
            None => false,
        }
    }

    pub fn render(&mut self, id: usize) {
        match self.actors.get_mut(id) {
            Some(Actor { facade, messages }) => {
                facade.render(self.id, id, *messages);
            }
            None => {}
        }
    }
}

pub struct FormatterHtml<TMessage: Clone + MyDebug> {
    pub app: usize,
    pub actor: usize,
    pub html: String,
    pub messages: Vec<MessageFactory<TMessage>>,
}

impl<TMessage: Clone + MyDebug> FormatterHtml<TMessage> {
    pub fn new(app: usize, actor: usize) -> Self {
        Self {
            app,
            actor,
            html: "".to_string(),
            messages: Vec::new(),
        }
    }
}

pub trait DisplayHtml {
    type Message: Clone + MyDebug;

    fn fmt(&self, f: &mut FormatterHtml<Self::Message>);

    fn render2(&mut self, app: usize, actor: usize, messages: *mut ()) -> Html {
        let v = unsafe { &mut *(messages as *mut Vec<MessageFactory<Self::Message>>) };
        v.clear();

        let mut f = FormatterHtml {
            app,
            actor,
            html: "".to_string(),
            messages: Vec::new(),
        };
        self.fmt(&mut f);

        // println!("HTML: {:?}", html);
        // println!("Messages: {:?}", messages);

        v.extend(f.messages);
        unsafe { apply_to(f.html.len() as u32, f.html.as_ptr() as u32) };

        f.html
    }
}
