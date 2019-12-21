use std::mem;
use std::rc::Rc;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Window, HtmlCanvasElement, 
    WebGlRenderingContext, WebGlBuffer,
    WebGlShader, WebGlProgram,
    Request, RequestInit, RequestMode, Response};

include!(concat!(env!("OUT_DIR"), "/hello.rs"));

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = printArray)]
    fn print_array(buffer: &js_sys::ArrayBuffer);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    log(&format!("Hello, {}!", name));
}

pub enum ErrorCode
{
    None,
    NotIterable,
}

pub struct Error
{
    code: ErrorCode,
    message: Option<String>,
}

impl From<ErrorCode> for Error { fn from(code: ErrorCode) -> Self { return Error{code: code, message: None}; } }

impl From<Error> for JsValue
{
    fn from(error: Error) -> Self 
    {
        return match error.message {
            Some(msg) => JsValue::from(msg.to_string()),
            None => match error.code {
                ErrorCode::None => JsValue::from(-1),
                ErrorCode::NotIterable => JsValue::from(-2),
            }
        }
    }
}

pub trait JsOk<T> { 
    fn jsok(self) -> Result<T, Error>; 
    fn jsok_msg(self, msg: String) -> Result<T, Error>;
}
impl<T> JsOk<T> for Option<T> { 
    fn jsok(self) -> Result<T, Error> { return self.ok_or(From::from(ErrorCode::None)); } 
    fn jsok_msg(self, msg: String) -> Result<T, Error> { 
        return self.ok_or(Error{code: ErrorCode::None, message: Some(msg) }); 
    }
}

type JsResult<T=JsValue> = Result<T,JsValue>;

fn jsok() -> JsResult { Ok(JsValue::from(1)) }

// fn as_iter(v : &JsValue) -> Result<js_sys::IntoIter,WASMFFIError> {
//     return js_sys::try_iter(v)
//         .or(Err(WASMFFIError::NotIterable))?
//         .ok_or(WASMFFIError::NotIterable);
// }

// #[wasm_bindgen]
// pub fn greet_n(names: &JsValue) -> JsResult {
//     for i in as_iter(names)? {        
//         let name = i?
//             .as_string();
//         match name {
//             Some(str) => log(&format!("Hello, {}!", str)),
//             None => log(&format!("<EMPTY>"))
//         }            
        
//     }

//     return Ok(JsValue::from(1));
// }



fn f32_array_buffer(items: Vec<f32>) -> js_sys::ArrayBuffer
{
    let arr = js_sys::Float32Array
        ::new_with_length(items.len() as u32);
    let mut i = 0;
    for x in items {
        arr.fill(x, i, i+1);
        i+=1;
    }
    arr.buffer()
}

fn u16_array_buffer(items: Vec<u16>) -> js_sys::ArrayBuffer
{
    let arr = js_sys::Uint16Array
        ::new_with_length(items.len() as u32);
    let mut i = 0;
    for x in items {
        arr.fill(x, i, i+1);
        i+=1;
    }
    arr.buffer()
}

fn setup_array_buffer(gl: &WebGlRenderingContext, buffer: &js_sys::ArrayBuffer)
    -> JsResult<WebGlBuffer>
{
    let glbuffer = gl.create_buffer().jsok()?;

    gl.bind_buffer(WebGlRenderingContext::ARRAY_BUFFER, 
        Some(&glbuffer));
    gl.buffer_data_with_opt_array_buffer(WebGlRenderingContext::ARRAY_BUFFER, 
        Some(&buffer), 
        WebGlRenderingContext::STATIC_DRAW);
    gl.bind_buffer(WebGlRenderingContext::ARRAY_BUFFER, 
        None);

    Ok(glbuffer)
}

fn setup_element_array_buffer(gl: &WebGlRenderingContext, buffer: &js_sys::ArrayBuffer)
    -> JsResult<WebGlBuffer>
{
    let glbuffer = gl.create_buffer().jsok()?;

    gl.bind_buffer(WebGlRenderingContext::ELEMENT_ARRAY_BUFFER, 
        Some(&glbuffer));
    gl.buffer_data_with_opt_array_buffer(WebGlRenderingContext::ELEMENT_ARRAY_BUFFER, 
        Some(&buffer),
        WebGlRenderingContext::STATIC_DRAW);
    gl.bind_buffer(WebGlRenderingContext::ELEMENT_ARRAY_BUFFER, 
        None);

    Ok(glbuffer)
}

enum ShaderType
{
    VertexShader,
    FragmentShader
}

fn setup_shader(gl: &WebGlRenderingContext, stype: ShaderType, code: &str) -> JsResult<WebGlShader>
{
    let t = match stype {
        ShaderType::VertexShader => WebGlRenderingContext::VERTEX_SHADER,
        ShaderType::FragmentShader => WebGlRenderingContext::FRAGMENT_SHADER,
    };
    let shader = gl.create_shader(t).jsok()?;
    gl.shader_source(&shader, code);
    gl.compile_shader(&shader);
    return Ok(shader);
}

fn link_shaders(gl:&WebGlRenderingContext, 
    vshader: Option<&WebGlShader>,
    fshader: Option<&WebGlShader>) -> JsResult<WebGlProgram>
{
    let vs = vshader.jsok()?;
    let fs = fshader.jsok()?;

    let program = gl.create_program().jsok()?;
    gl.attach_shader(&program, vs);
    gl.attach_shader(&program, fs);
    gl.link_program(&program);

    Ok(program)
}

async fn fetch_get_octet(url: &str) -> Result<js_sys::ArrayBuffer, JsValue>
{
    let mut opts = RequestInit::new();
    opts.method("GET");
    opts.mode(RequestMode::Cors);

    let request = Request::new_with_str_and_init(url, &opts)?;
    request
        .headers()
        .set("Accept", "application/octet-stream")?;

    let window = web_sys::window().unwrap();
    let req = window.fetch_with_request(&request);
    let r = JsFuture::from(req).await?;
    let resp: Response = r.dyn_into().unwrap();
    let buffer : js_sys::ArrayBuffer = 
        JsFuture::from(resp.array_buffer()?)
        .await?.dyn_into()?;
    Ok(buffer)
}

trait AsArrayBuffer
{
    fn as_array_buffer(self) ->  js_sys::ArrayBuffer;
}

impl AsArrayBuffer for Vec<f32> { fn as_array_buffer(self) ->  js_sys::ArrayBuffer { f32_array_buffer(self) } }
impl AsArrayBuffer for Vec<u16> { fn as_array_buffer(self) ->  js_sys::ArrayBuffer { u16_array_buffer(self) } }

enum Component
{
    WebGL(WebGlComponent)
}

struct WebGlComponent
{
    gl: Rc<WebGlRenderingContext>,
    vertex_buffer: Option<WebGlBuffer>,
    index_buffer: Option<WebGlBuffer>,
    ilen: Option<i32>,
    vshader: Option<WebGlShader>,
    fshader: Option<WebGlShader>,
    program: Option<WebGlProgram>,
}

impl WebGlComponent
{
    pub fn new (gl: Rc<WebGlRenderingContext>) -> WebGlComponent
    {
        WebGlComponent {
            gl: gl, 
            vertex_buffer: None,
            index_buffer: None,
            ilen: None,

            vshader: None,
            fshader: None,
            program: None
        }
    }

    pub fn as_component(self) -> Component { Component::WebGL(self) }

    pub fn push_vertices(&mut self, buffer: &js_sys::ArrayBuffer) -> JsResult
    {
        let vb = setup_array_buffer(&self.gl, buffer)?;
        self.vertex_buffer = Some(vb);
        
        jsok()
    }

    pub fn push_indices(&mut self, buffer: &js_sys::ArrayBuffer) -> JsResult
    {
        let ib = setup_element_array_buffer(&self.gl, buffer)?;
        self.index_buffer = Some(ib);
        self.ilen = Some(buffer.byte_length() as i32 / (mem::size_of::<u32>() as i32));

        jsok()
    }

    pub fn compile_vertex_shader(&mut self, code: &str) -> JsResult {
        let shader = setup_shader(&self.gl, ShaderType::VertexShader, code)?;
        self.vshader = Some(shader);
        jsok()
    }

    pub fn compile_fragment_shader(&mut self, code: &str) -> JsResult {
        let shader = setup_shader(&self.gl, ShaderType::FragmentShader, code)?;
        self.fshader = Some(shader);
        jsok()
    }

    pub fn link_shaders(&mut self) -> JsResult {
        let program = link_shaders(&self.gl, 
            self.vshader.as_ref(), 
            self.fshader.as_ref())?;
        self.program = Some(program);
        jsok()
    }

    pub fn render(&self)
    {
        if self.vertex_buffer.is_none() { return; }
        if self.index_buffer.is_none() { return; }
        if self.ilen.is_none() { return; }

        let gl = &*self.gl;

        let shader_program = self.program
            .as_ref()
            .unwrap();
        gl.use_program(self.program.as_ref());
        let coord = gl.get_attrib_location(shader_program, "coordinates") as u32;

        gl.bind_buffer(WebGlRenderingContext::ARRAY_BUFFER,
            self.vertex_buffer.as_ref());
        gl.vertex_attrib_pointer_with_i32(coord, 
            3, WebGlRenderingContext::FLOAT, 
            false, 
            0, 0);
        gl.enable_vertex_attrib_array(coord);

        gl.bind_buffer(WebGlRenderingContext::ELEMENT_ARRAY_BUFFER, 
                self.index_buffer.as_ref());
        let ilen = self.ilen.unwrap();
        gl.draw_elements_with_i32(WebGlRenderingContext::TRIANGLES, 
            6, 
            WebGlRenderingContext::UNSIGNED_SHORT,
            0);
    }
}

struct Entity
{
    components: Vec<Component>
}

impl Entity {
    pub fn new () -> Entity {
        Entity { components: Vec::with_capacity(8) }
    }

    pub fn push_component(&mut self, component: Component)
    {
        self.components.push(component);
    }
}

#[wasm_bindgen]
pub struct Game
{
    gl: Rc<WebGlRenderingContext>,
    entities: Vec<Entity>,
}

#[wasm_bindgen]
impl Game
{
    #[wasm_bindgen(constructor)]
    pub fn new(canvas_id: &JsValue) -> JsResult<Game> {
        let cid = canvas_id.as_string().jsok()?;
        let window = web_sys::window().jsok()?.dyn_into::<Window>()?;
        let document = window
            .document().jsok()?;
        let canvas = document
            .get_element_by_id(&cid)
            .jsok_msg(format!("canvas not found: id=[{}]", cid))?
            .dyn_into::<HtmlCanvasElement>()?;
        let webgl = canvas
            .get_context("webgl")?
            .jsok_msg(format!("webgl not supported: id=[{}]", cid))?
            .dyn_into::<WebGlRenderingContext>()?;
        webgl.viewport(0, 0,
            canvas.width() as i32,
            canvas.height() as i32);
        let gl = Rc::new(webgl);

        let mut game = Game { gl: gl.clone(), entities: Vec::new() };

        let mut cube = Entity::new();
        let mut cubegl = WebGlComponent::new(gl.clone());
        let vertices = vec![
            -0.5, 0.5, 0.0,
            -0.5,-0.5, 0.0,
             0.5,-0.5, 0.0,
             0.5, 0.5, 0.0
           ].as_array_buffer();
        print_array(&vertices);
        cubegl.push_vertices(&vertices)?;
        let indices = vec![3,2,1,3,1,0]
            .as_array_buffer();
        print_array(&indices);
        cubegl.push_indices(&indices)?;
        cubegl.compile_vertex_shader("attribute vec3 coordinates;
void main(void) {
    gl_Position = vec4(coordinates, 1.0);
}")?;
        cubegl.compile_fragment_shader("
void main(void) {
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1);
}")?;
        cubegl.link_shaders()?;
        cube.push_component(cubegl.as_component());
        
        game.entities.push(cube);

        message();
        Ok(game)
    }

    pub async fn load(self)  -> Result<Game, JsValue>
    {
        let promise = js_sys::Promise::resolve(&42.into());
        let result = wasm_bindgen_futures::JsFuture::from(promise).await?;

        let hair_vx = fetch_get_octet("data/ty/Boy01_Hair_Geo-lib-Position").await?;

        let mut cube = Entity::new();
        let mut cubegl = WebGlComponent::new(self.gl.clone());
        cubegl.push_vertices(&hair_vx)?;
        let indices = vec![3,2,1,3,1,0]
            .as_array_buffer();
        print_array(&indices);
        cubegl.push_indices(&indices)?;
        cubegl.compile_vertex_shader("attribute vec3 coordinates;
void main(void) {
    gl_Position = vec4(coordinates, 1.0);
}")?;
        cubegl.compile_fragment_shader("
void main(void) {
    gl_FragColor = vec4(1.0, 0.0, 0.0, 1);
}")?;
        cubegl.link_shaders()?;
        cube.push_component(cubegl.as_component());

        Ok(self)
    }

    pub fn render(&self, _timestamp: f64)
    {
        let gl = &self.gl;
        gl.clear_color(0.0, 0.0, 0.0, 1.0);
        gl.clear(WebGlRenderingContext::COLOR_BUFFER_BIT);
        gl.viewport(0, 0, 640, 480);

        for entity in &self.entities {
            for component in &entity.components {
                match component {
                    Component::WebGL(data) => {
                        data.render();
                    }
                }

            }
        }
    }
}