#[derive(Clone, Debug)]
pub enum Actions {
    Login { username: String, password: String },
}
