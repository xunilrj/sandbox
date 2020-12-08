use runtime::*;

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

impl std::default::Default for CounterState {
    fn default() -> Self {
        CounterState {
            count: 0,
            amount: "1".into(),
        }
    }
}

impl runtime::ApplicationTrait for CounterState {
    type State = Self;
    type Message = Messages;
    type Actions = crate::actions::Actions;

    fn update(&mut self, message: &Self::Message, commands: &mut Vec<Self::Actions>) {
        match message {
            Messages::Increment => {
                self.count = {
                    let v = self.amount.parse::<u64>().unwrap_or(0);
                    self.count.saturating_add(v)
                }
            }
            Messages::Decrement => {
                self.count = {
                    let v = self.amount.parse::<u64>().unwrap_or(0);
                    self.count.saturating_sub(v)
                }
            }
            Messages::Input { data } => {
                self.amount.value.push_str(data);
            }
        }
    }
}

impl runtime::DisplayHtml for CounterState {
    type Message = Messages;
    fn fmt(&self, f: &mut runtime::FormatterHtml<Self::Message>) {
        html::html! {
            <div id="root">
                <div>
                    <button class="red" onclick={Messages::Increment}>"Increment"</button>
                    <button onclick={Messages::Decrement}>"Decrement"</button>
                </div>
                <div>
                    <input value={self.amount.value} oninput={|e| Messages::Input { data: e.data.to_string()}} />
                </div>
                <div>
                    <div>{self.count}</div>
                </div>
            </div>
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use runtime::*;

//     #[test]
//     fn it_works() {
//         let mut t = runtime::Tester::<CounterState>::new();

//         t.click("#root button:nth-of-type(1)");
//         t.assert_inner_text("#root div div", |x| x == "1");

//         t.click("#root button:nth-of-type(2)");
//         t.assert_inner_text("#root div div", |x| x == "0");
//     }
// }
