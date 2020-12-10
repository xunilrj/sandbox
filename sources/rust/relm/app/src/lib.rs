pub mod actions;
mod counter;
mod login;
use runtime::*;

mount! {
    0 => counter::CounterState,
    1 => login::State
}
