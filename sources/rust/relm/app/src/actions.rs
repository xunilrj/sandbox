#[derive(Clone, Debug)]
pub enum Actions {
    Timeout { duration: std::time::Duration },
    TryLogin { username: String, password: String },
}
