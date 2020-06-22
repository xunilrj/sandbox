use console_error_panic_hook;
use tiny_ecs::ECSError;
use tiny_ecs::Entities;
use wasm_bindgen::prelude::*;
use web_sys::window;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn setup() {
    console_error_panic_hook::set_once();
    wasm_logger::init(wasm_logger::Config::default());

    let mut world = Entities::new(Some(1000), Some(10));
    let ent = world.new_entity().finalise().unwrap();

    let w = window().unwrap();
    w.
}
