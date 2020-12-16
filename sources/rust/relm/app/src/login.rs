use runtime::*;

#[derive(Clone)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[allow(dead_code)]
pub enum StateErrors {
    UsernameEmpty,
    PasswordEmpty,
    Timeout,
    InvalidUsernameAndPassword,
}

impl std::fmt::Display for StateErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            StateErrors::UsernameEmpty => write!(f, "Username Empty"),
            StateErrors::PasswordEmpty => write!(f, "Password Empty"),
            StateErrors::Timeout => write!(f, "Timeout"),
            StateErrors::InvalidUsernameAndPassword => write!(f, "Invalid Username and Password"),
        }
    }
}

#[derive(Default)]
pub struct State {
    pub errors: Vec<StateErrors>,
    pub username: InputState,
    pub password: InputState,
    pub loading: bool,
}

impl State {
    fn validate(&mut self) {
        if self.username.is_empty() {
            self.errors.push(StateErrors::UsernameEmpty);
        }
        if self.password.is_empty() {
            self.errors.push(StateErrors::PasswordEmpty);
        }
    }
}

#[derive(Clone)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[allow(dead_code)]
pub enum Messages {
    InputLogin { data: String },
    InputPassword { data: String },
    Login,
    LoginFailed(StateErrors),
    LastActionTimedout,
}

#[derive(Clone)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
pub enum Actions {
    SetTimeout { duration: std::time::Duration },
    TryLogin { username: String, password: String },
}

impl Actions {
    pub async fn handle(self, mut env: runtime::env::Env<Messages>) {
        match self {
            Actions::SetTimeout { duration } => {
                env.set_timeout(duration).await;
                env.send(crate::login::Messages::LastActionTimedout);
            }
            Actions::TryLogin { .. } => {}
        }
    }
}

impl runtime::UpdatableState for State {
    type State = Self;
    type Message = Messages;
    type Actions = Actions;

    fn update(&mut self, message: &Self::Message, actions: &mut Vec<Self::Actions>) {
        self.errors.clear();

        if !self.loading {
            match message {
                Messages::LastActionTimedout | Messages::LoginFailed { .. } => {}
                Messages::InputLogin { data } => {
                    self.username.value.push_str(data);
                }
                Messages::InputPassword { data } => {
                    self.password.value.push_str(data);
                }
                Messages::Login => {
                    self.validate();

                    if self.errors.is_empty() {
                        self.loading = true;
                        actions.push(Actions::TryLogin {
                            username: self.username.value.clone(),
                            password: self.password.value.clone(),
                        });
                        actions.push(Actions::SetTimeout {
                            duration: std::time::Duration::from_secs(5),
                        });
                    }
                }
            }
        } else {
            match message {
                Messages::InputLogin { .. } | Messages::InputPassword { .. } | Messages::Login => {}
                Messages::LastActionTimedout => {
                    self.errors.push(StateErrors::Timeout);
                    self.password.clear();
                    self.loading = false;
                }
                Messages::LoginFailed(error) => {
                    self.errors.push(error.clone());
                    self.password.clear();
                    self.loading = false;
                }
            }
        }
    }
}

html::html! {State
    <div id="root">
        {self.errors.map(|x| {<div class="error">{x}</div>})}
        {if self.loading {
            <div>"Loading"</div>
        } else {
            <div>
                <input ref="username" value={self.username.value} oninput={|e| Messages::InputLogin { data: e.data.to_string()}} />
            </div>
            <div>
                <input ref="password" value={self.password.value} oninput={|e| Messages::InputPassword { data: e.data.to_string()}} />
            </div>
            <button ref="login" onclick={Messages::Login}>"Login"</button>
        }}
    </div>
}

#[cfg(test)]
mod tests {
    use super::*;
    use runtime::tester::*;

    #[test]
    fn must_show_username_empty_error() {
        let mut t = Tester::<State>::new();

        t.get_password().unwrap().raise(OnInput { data: any() });
        t.get_login().unwrap().raise(OnClick {});

        assert!(matches!(
            t.state.errors.assert_single(),
            StateErrors::UsernameEmpty
        ));
        t.query_class_error().assert_single();
    }

    #[test]
    fn must_show_password_empty_error() {
        let mut t = Tester::<State>::new();

        t.get_username().unwrap().raise(OnInput { data: any() });
        t.get_login().unwrap().raise(OnClick {});

        assert!(matches!(
            t.state.errors.assert_single(),
            StateErrors::PasswordEmpty
        ));
        t.query_class_error().assert_single();
    }

    #[test]
    fn must_show_timeout_error() {
        let mut t = runtime::tester::Tester::<State>::new();

        t.get_username().unwrap().raise(OnInput { data: any() });
        t.get_password().unwrap().raise(OnInput { data: any() });
        t.get_login().unwrap().raise(OnClick {});

        assert!(t.state.loading);
        assert_eq!(t.actions.len(), 2);

        t.handle_action(|a| match a {
            Actions::SetTimeout { .. } => Ok(Messages::LastActionTimedout),
            _ => Err(()),
        })
        .expect("To work");

        assert!(matches!(
            t.state.errors.assert_single(),
            StateErrors::Timeout
        ));
        t.query_class_error().assert_single();
    }

    #[test]
    fn must_show_failed_login_error() {
        let mut t = runtime::tester::Tester::<State>::new();

        t.get_username().unwrap().raise(OnInput { data: any() });
        t.get_password().unwrap().raise(OnInput { data: any() });
        t.get_login().unwrap().raise(OnClick {});

        assert!(t.state.loading);
        assert_eq!(t.actions.len(), 2);

        t.handle_action(|a| match a {
            Actions::TryLogin { .. } => Ok(Messages::LoginFailed(
                StateErrors::InvalidUsernameAndPassword,
            )),
            _ => Err(()),
        })
        .expect("To work");

        assert!(matches!(
            t.state.errors.assert_single(),
            StateErrors::InvalidUsernameAndPassword
        ));
        assert!(t.state.password.is_empty());
        t.query_class_error().assert_single();
    }
}
