use runtime::*;
pub type App = Application<CounterState, Messages>;

pub struct CounterState {
    pub count: u64,
    pub amount: InputState,
}

#[derive(Debug, Clone)]
pub enum Messages {
    Increment,
    Decrement,
    Input { data: String },
}

pub fn init() -> CounterState {
    CounterState {
        count: 0,
        amount: "1".into(),
    }
}

pub fn update(message: &Messages, state: &mut CounterState) {
    if let Ok(v) = state.amount.parse::<u64>() {
        match message {
            Messages::Increment => state.count = state.count.saturating_add(v),
            Messages::Decrement => state.count = state.count.saturating_sub(v),
            Messages::Input { data } => {
                state.amount.value.push_str(data);
            }
        }
    } else {
        //todo show error
    }
}

pub fn view(state: &CounterState) -> ViewResult<Messages> {
    html::html! {
        <div id="root">
            <div>
                <button class="red" onclick={Messages::Increment}>"Increment"</button>
                <button onclick={Messages::Decrement}>"Decrement"</button>
            </div>
            <div>
                <input value={state.amount.value} oninput={|e| Messages::Input { data: e.data.to_string()}} />
            </div>
            <div>
                <div>{state.count}</div>
            </div>
        </div>
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut app = Application::new(init, update, view);
        app.render();
        app.send_by_id(0, Vec::new()).unwrap();
        app.render();
        app.send_by_id(1, Vec::new()).unwrap();
        app.render();
    }
}
