use runtime::*;

#[derive(Default)]
pub struct State {
    pub username: InputState,
    pub password: InputState,
    pub loading: bool,
}

#[derive(Debug, Clone)]
pub enum Messages {
    InputLogin { data: String },
    InputPassword { data: String },
    Login,
}

impl runtime::ApplicationTrait for State {
    type State = Self;
    type Message = Messages;
    type Actions = crate::actions::Actions;

    fn update(&mut self, message: &Self::Message, actions: &mut Vec<Self::Actions>) {
        if !self.loading {
            match message {
                Messages::InputLogin { data } => {
                    self.username.value.push_str(data);
                }
                Messages::InputPassword { data } => {
                    self.password.value.push_str(data);
                }
                Messages::Login => {
                    self.loading = true;
                    actions.push(crate::actions::Actions::Login {
                        username: self.username.value.clone(),
                        password: self.password.value.clone(),
                    })
                }
            }
        }
    }

    fn to_html(&self) -> runtime::ViewResult<Messages> {
        html::html! {
            <div id="root">
                {if self.loading {
                    <div>"Loading"</div>
                } else {
                    <div>
                        <input value={self.username.value} oninput={|e| Messages::InputLogin { data: e.data.to_string()}} />
                    </div>
                    <div>
                        <input value={self.password.value} oninput={|e| Messages::InputPassword { data: e.data.to_string()}} />
                    </div>
                    <button onclick={Messages::Login}>"Login"</button>
                }}
            </div>
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let mut t = runtime::Tester::<super::State>::new();

        t.input("#root > div > input", 0, "user1");
        t.input("#root > div > input", 1, "password1");

        t.click("#root button", 0);
        // t.assert_inner_text("#root div div", |x| x == "1");

        // t.click("#root button:nth-of-type(2)");
        // t.assert_inner_text("#root div div", |x| x == "0");
    }
}
