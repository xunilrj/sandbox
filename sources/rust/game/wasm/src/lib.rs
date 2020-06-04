mod abi;
mod utils;

use abi::*;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn init(level: LogLevel) -> JsResult<()> {
    console_log::init_with_level(level.0).context(&"Failed to start log")?;
    log::warn!("Log Enabled at [{}]", level.0);
    Ok(())
}

#[wasm_bindgen]
pub fn greet() {
    log::info!("Hello, wasm 2!");
}
