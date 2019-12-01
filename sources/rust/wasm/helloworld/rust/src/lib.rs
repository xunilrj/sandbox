use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    log(&format!("Hello, {}!", name));
}

enum WASMFFIError
{
    NotIterable
}

impl From<WASMFFIError> for JsValue
{
    fn from(error: WASMFFIError) -> Self 
    {
        match error {
            WASMFFIError::NotIterable => JsValue::from(0)
        }
    }
}

type JsResult = Result<JsValue,JsValue>;

fn as_iter(v : &JsValue) -> Result<js_sys::IntoIter,WASMFFIError> {
    return js_sys::try_iter(v)
        .or(Err(WASMFFIError::NotIterable))?
        .ok_or(WASMFFIError::NotIterable);
}

#[wasm_bindgen]
pub fn greet_n(names: &JsValue) -> JsResult {
    for i in as_iter(names)? {        
        let name = i?
            .as_string();
        match name {
            Some(str) => log(&format!("Hello, {}!", str)),
            None => log(&format!("<EMPTY>"))
        }            
        
    }

    return Ok(JsValue::from(1));
}