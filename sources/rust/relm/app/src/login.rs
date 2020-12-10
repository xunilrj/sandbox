use crate::actions;
use actions::*;
use runtime::*;

#[derive(Default)]
pub struct State {
    pub errors: Vec<String>,
    pub username: InputState,
    pub password: InputState,
    pub loading: bool,
}

#[derive(Debug, Clone)]
pub enum Messages {
    InputLogin { data: String },
    InputPassword { data: String },
    Login,
    LoginFailed { error: String },
    Timeout,
}

impl runtime::UpdatableState for State {
    type State = Self;
    type Message = Messages;
    type Actions = crate::actions::Actions;

    fn update(&mut self, message: &Self::Message, actions: &mut Vec<Self::Actions>) {
        self.errors.clear();

        if !self.loading {
            match message {
                Messages::Timeout | Messages::LoginFailed { .. } => {}
                Messages::InputLogin { data } => {
                    self.username.value.push_str(data);
                }
                Messages::InputPassword { data } => {
                    self.password.value.push_str(data);
                }
                Messages::Login => {
                    if self.username.is_empty() {
                        self.errors.push("Username empty".to_string());
                    }
                    if self.username.is_empty() {
                        self.errors.push("Password empty".to_string());
                    }

                    if self.errors.len() == 0 {
                        self.loading = true;
                        actions.push(Actions::TryLogin {
                            username: self.username.value.clone(),
                            password: self.password.value.clone(),
                        });
                        actions.push(Actions::Timeout {
                            duration: std::time::Duration::from_secs(5),
                        });
                    }
                }
            }
        } else {
            match message {
                //TODO maybe update must return Result
                Messages::InputLogin { .. } | Messages::InputPassword { .. } | Messages::Login => {}
                Messages::Timeout => {
                    self.errors
                        .push("Ops, timeout! Please try again!".to_string());
                    self.password.clear();
                    self.loading = false;
                }
                Messages::LoginFailed { error } => {
                    self.errors.push(error.clone());
                    self.password.clear();
                    self.loading = false;
                }
            }
        }
    }
}

impl runtime::DisplayHtml for State {
    type Message = Messages;
    fn fmt(&self, f: &mut runtime::FormatterHtml<Self::Message>) {
        html::html! {
            <div id="root">
                {self.errors.map(|x| {<div>{x}</div>})}
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
    use super::*;

    #[test]
    fn it_works() {
        let mut t = runtime::tester::Tester::<State>::new();

        // t.input("#root > div > input", 0, "user1");
        // t.input("#root > div > input", 1, "password1");
        t.click("#root button", 0);

        // assert!(t.state.loading);
        // assert_eq!(t.actions.len(), 2);

        // assert!(matches!(t.get_action(0),
        //     Some(Actions::TryLogin { username, password }) if
        //         username == "user1" &&
        //         password == "password1"
        // ));

        // assert!(matches!(t.get_action(1),
        //     Some(Actions::Timeout { duration }) if
        //         duration.as_secs() == 5
        // ));

        // // t.handle_action(1, |a| match a {
        // //     Actions::Timeout { .. } => Ok(Messages::Timeout),
        // //     _ => Err(()),
        // // })
        // // .expect("To work");

        // t.handle_action(0, |a| match a {
        //     Actions::TryLogin { username, password } => Ok(Messages::LoginFailed {
        //         error: "User does not exist".to_string(),
        //     }),
        //     _ => Err(()),
        // })
        // .expect("To work");

        // assert!(t.state.password.is_empty());
    }
}
