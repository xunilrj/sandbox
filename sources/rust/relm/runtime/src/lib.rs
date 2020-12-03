use std::sync::RwLock;

type InitFunction<T> = fn() -> T;
type UpdateFunction<TMessage, TState> = fn(message: &TMessage, state: &mut TState);
type ViewFunction<TState, TMessage> = fn(&TState) -> (Html, Vec<TMessage>);

type Html = String;

struct Application<TState, TMessage> {
    state: TState,
    update: UpdateFunction<TMessage, TState>,
    view: ViewFunction<TState, TMessage>,
    messages: Vec<TMessage>,
}

impl<TState, TMessage: std::fmt::Debug> Application<TState, TMessage> {
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

    pub fn send_by_id(&mut self, id: usize) -> std::result::Result<(), u64> {
        let message = self.messages.get_mut(id).ok_or(0u64)?;

        let f = self.update;
        f(message, &mut self.state);

        self.render();

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

mod login {
    use super::*;
    pub type App = super::Application<State, Messages>;

    pub struct State {
        pub count: u64,
    }

    #[derive(Debug)]
    pub enum Messages {
        Increment,
        Decrement,
    }

    pub fn init() -> State {
        State { count: 0 }
    }

    pub fn update(message: &Messages, state: &mut State) {
        match message {
            Messages::Increment => state.count += 1,
            Messages::Decrement => state.count -= 1,
        }
    }

    pub fn view(state: &State) -> (Html, Vec<Messages>) {
        html::html! {
            <div id="root">
                <div>
                    <button class="red" onclick={Messages::Increment}>"Increment"</button>
                    <button onclick={Messages::Decrement}>"Decrement"</button>
                </div>
                <div>
                    <input />
                </div>
                <div>
                    <div>{state.count}</div>
                </div>
            </div>
        }
    }
}

enum Applications {
    Login(login::App),
}

impl Applications {
    pub fn send_by_id(&mut self, id: usize) -> std::result::Result<(), u64> {
        match self {
            Applications::Login(app) => app.send_by_id(id),
        }
    }

    pub fn render(&mut self) -> Html {
        match self {
            Applications::Login(app) => app.render(),
        }
    }
}

mod spinlock;
use spinlock::SpinLock;
static mut APPS: SpinLock<Vec<Applications>> = SpinLock::new(Vec::new());

extern "C" {
    fn apply_to(size: u32, ptr: u32);
}

#[no_mangle]
pub fn mount() -> u64 {
    let app = login::App::new(login::init, login::update, login::view);

    unsafe {
        let mut l = APPS.lock();
        l.push(Applications::Login(app));
        let id = l.len() as u64 - 1;

        match l.get_mut(id as usize) {
            Some(app) => {
                app.render();
            }
            None => {}
        }

        id
    }
}

#[no_mangle]
pub fn send(app_idx: u64, msg_idx: u64) -> bool {
    unsafe {
        let mut l = APPS.lock();
        let app = l.get_mut(app_idx as usize).unwrap();
        match app.send_by_id(msg_idx as usize) {
            Ok(_) => true,
            Err(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::login::*;
    use super::*;
    use crate::Application;

    #[test]
    fn it_works() {
        let mut app = Application::new(init, update, view);
        app.render();
        app.send_by_id(0).unwrap();
        app.render();
        app.send_by_id(1).unwrap();
        app.render();
    }
}
