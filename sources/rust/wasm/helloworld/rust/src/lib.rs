use std::mem;
use std::rc::Rc;
use std::f64;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
use js_sys::{Uint8Array};
use web_sys::{Window, HtmlCanvasElement, 
    WebGlRenderingContext, WebGlBuffer,
    WebGlShader, WebGlProgram,
    Request, RequestInit, RequestMode, Response,
    WebGlTexture};

include!(concat!(env!("OUT_DIR"), "/hello.rs"));

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = printArray)]
    fn print_array(buffer: &js_sys::ArrayBuffer, t: &str);
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
    WebGL(WebGlComponent),
}



struct WebGlComponent
{
    gl: Rc<WebGlRenderingContext>,
    buffers: Vec<(WebGlBuffer, String, u32, i32)>,
    texs: Vec<(String, WebGlTexture)>,
    index_buffer: Option<WebGlBuffer>,
    ilen: Option<i32>,
    vshader: Option<WebGlShader>,
    fshader: Option<WebGlShader>,
    program: Option<WebGlProgram>,

    model: [f32;16],
}

fn bind_float_buffer(gl: &WebGlRenderingContext,
    program: &WebGlProgram, 
    buffer_type: u32,
    buffer: &WebGlBuffer,
    name: &str,
    stride: i32)
{
    let i = gl.get_attrib_location(program, name) as i32;
    if i == -1 { log(&format!("attrib not found {}", name)); return; }

    gl.bind_buffer(WebGlRenderingContext::ARRAY_BUFFER, Some(buffer));
    gl.vertex_attrib_pointer_with_i32(i as u32, stride,
        buffer_type, 
        false,
        0, 0);
    gl.enable_vertex_attrib_array(i as u32);
}

fn add(a: [f32;3], b: [f32;3]) -> [f32;3]
{
    [a[0]+b[0],a[1]+b[1],a[2]+b[2]]
}

fn sub(a: [f32;3], b: [f32;3]) -> [f32;3]
{
    [a[0]-b[0],a[1]-b[1],a[2]-b[2]]
}

fn scale3(a: [f32;3], f: f32) -> [f32;3]
{
    [a[0]*f,a[1]*f,a[2]*f]
}

fn normalize(a: [f32;3]) -> [f32;3]
{
    let l = (a[0]*a[0]+a[1]*a[1]+a[2]*a[2]).sqrt();
    [a[0]/l,a[1]/l,a[2]/l]
}

fn dot(a: [f32;3], b: [f32;3]) -> f32
{
    a[0]*b[0]+a[1]*b[1]+a[2]*b[2]
}

fn cross(a: [f32;3], b: [f32;3]) -> [f32;3]
{
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0]
    ]
}

fn id_matrix() -> [f32;16]
{
    return [
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    ];
}

fn lookat_matrix(from: [f32;3],
    to: [f32;3],
    up: [f32;3]) -> [f32;16]
{
    let f = normalize(sub(from, to));
    let r = cross(normalize(up), f);
    let u = cross(f, r);
    // return [
    //     r[0],    r[1],    r[2], 0.0,
    //     u[0],    u[1],    u[2], 0.0,
    //     f[0],    f[1],    f[2], 0.0,
    //  from[0], from[1], from[2], 1.0
    // ];
    return [
        r[0], u[0], f[0], 0.0,
        r[1], u[1], f[1], 0.0,
        r[2], u[2], f[2], 0.0,
-dot(r,from), -dot(u,from),  -dot(f,from),     1.0
    ];
}

fn frustum_matrix(left: f32, right: f32,
    bottom: f32, top: f32, 
    near: f32, far: f32) -> [f32;16]
{
    let sx = 2.0 * near / (right - left);
    let sy = 2.0 * near / (top - bottom);
    let c2 = - (far + near) / (far - near);
    let c1 = 2.0 * near * far / (near - far);
    let tx = -near * (left + right) / (right - left);
    let ty = -near * (bottom + top) / (top - bottom);

    [
        sx, 0.0, 0.0,  0.0,
        0.0, sy,  0.0,  0.0,
        0.0, 0.0,  c2, -1.0,
        tx, ty, c1, 0.0
    ]
}

fn perspective_matrix(fovy_radians: f32, 
    aspect: f32,
    near: f32,
    far: f32) -> [f32;16]
{
    let half_fovy = fovy_radians / 2.0;
    let top = near * half_fovy.tan();
    let bottom = -top;
    let right = top * aspect;
    let left = -right;
    frustum_matrix(left, right, bottom, top, near, far)
}

fn mul(a: [f32;16], b: [f32;16]) -> [f32;16]
{
    let mut r: [f32;16] = [
        0.0,0.0,0.0,0.0,
        0.0,0.0,0.0,0.0,
        0.0,0.0,0.0,0.0,
        0.0,0.0,0.0,0.0
    ];
    r[0]  = a[0] * b[0]  + a[4] * b[1]  + a[8]  * b[2]  + a[12] * b[3];
    r[1]  = a[1] * b[0]  + a[5] * b[1]  + a[9]  * b[2]  + a[13] * b[3];
    r[2]  = a[2] * b[0]  + a[6] * b[1]  + a[10] * b[2]  + a[14] * b[3];
    r[3]  = a[3] * b[0]  + a[7] * b[1]  + a[11] * b[2]  + a[15] * b[3];

    r[4]  = a[0] * b[4]  + a[4] * b[5]  + a[8]  * b[6]  + a[12] * b[7];
    r[5]  = a[1] * b[4]  + a[5] * b[5]  + a[9]  * b[6]  + a[13] * b[7];
    r[6]  = a[2] * b[4]  + a[6] * b[5]  + a[10] * b[6]  + a[14] * b[7];
    r[7]  = a[3] * b[4]  + a[7] * b[5]  + a[11] * b[6]  + a[15] * b[7];

    r[8]  = a[0] * b[8]  + a[4] * b[9]  + a[8]  * b[10] + a[12] * b[11];
    r[9]  = a[1] * b[8]  + a[5] * b[9]  + a[9]  * b[10] + a[13] * b[11];
    r[10] = a[2] * b[8]  + a[6] * b[9]  + a[10] * b[10] + a[14] * b[11];
    r[11] = a[3] * b[8]  + a[7] * b[9]  + a[11] * b[10] + a[15] * b[11];

    r[12] = a[0] * b[12] + a[4] * b[13] + a[8]  * b[14] + a[12] * b[15];
    r[13] = a[1] * b[12] + a[5] * b[13] + a[9]  * b[14] + a[13] * b[15];
    r[14] = a[2] * b[12] + a[6] * b[13] + a[10] * b[14] + a[14] * b[15];
    r[15] = a[3] * b[12] + a[7] * b[13] + a[11] * b[14] + a[15] * b[15];
    r
}

fn transpose(a: [f32;16]) -> [f32;16]
{
    [
        a[0], a[4], a[8], a[12],
        a[1], a[5], a[9], a[13],
        a[2], a[6], a[10], a[14],
        a[3], a[7], a[11], a[15],
    ]
}

impl WebGlComponent
{
    pub fn new (gl: Rc<WebGlRenderingContext>) -> WebGlComponent
    {
        WebGlComponent {
            gl: gl, 
            buffers: Vec::new(),
            texs: Vec::new(),
            index_buffer: None,
            ilen: None,

            vshader: None,
            fshader: None,
            program: None,

            model: id_matrix()
        }
    }

    pub fn as_component(self) -> Component { Component::WebGL(self) }

    pub fn push_buffer(&mut self, name: &str, stride: i32, buffer: &js_sys::ArrayBuffer) -> JsResult
    {
        let vb = setup_array_buffer(&self.gl, buffer)?;
        self.buffers.push(
            (vb, name.to_string(), WebGlRenderingContext::FLOAT, stride)
        );
        jsok()
    }

    pub fn push_indices(&mut self, buffer: &js_sys::ArrayBuffer) -> JsResult
    {
        let ib = setup_element_array_buffer(&self.gl, buffer)?;
        self.index_buffer = Some(ib);
        self.ilen = Some(buffer.byte_length() as i32 / (mem::size_of::<u16>() as i32));

        jsok()
    }

    pub fn push_tex(&mut self,
        name: &str,
        size: i32,
        pixels: &js_sys::ArrayBuffer) -> JsResult
    {
        let gl = &*self.gl;
        let texture = gl.create_texture()
            .jsok()?;
        gl.bind_texture(WebGlRenderingContext::TEXTURE_2D, Some(&texture));
        let level = 0;
        let internal_format: u32 = WebGlRenderingContext::RGBA;
        let width = size;
        let height = size;
        let border = 0;
        let src_format = WebGlRenderingContext::RGBA;
        let src_type = WebGlRenderingContext::UNSIGNED_BYTE;
        gl.tex_image_2d_with_i32_and_i32_and_i32_and_format_and_type_and_opt_array_buffer_view(
            WebGlRenderingContext::TEXTURE_2D, 
            level, internal_format as i32,
            width, height, border, 
            src_format, src_type,
            Some(&Uint8Array::new(pixels))
        )?;
        gl.generate_mipmap(WebGlRenderingContext::TEXTURE_2D);
        gl.bind_texture(WebGlRenderingContext::TEXTURE_2D, None);

        self.texs.push(
            (name.to_string(), texture)
        );
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

    pub fn render(&self, game: &Game)
    {
        if self.buffers.len() == 0 { return; }
        if self.index_buffer.is_none() { return; }
        if self.ilen.is_none() { return; }

        let gl = &*self.gl;

        let shader_program = self.program
            .as_ref()
            .unwrap();
        gl.use_program(self.program.as_ref());

        let mut texi = 0;
        for (name, tex) in &self.texs {
            gl.active_texture(WebGlRenderingContext::TEXTURE0 + texi);
            gl.bind_texture(WebGlRenderingContext::TEXTURE_2D, Some(tex));
            let i = gl.get_uniform_location(shader_program, name);
            gl.uniform1i(i.as_ref(), texi as i32);

            texi += 1;
        }

        for (buffer, name, buffer_type, stride) in &self.buffers {
            bind_float_buffer(gl, 
                shader_program, *buffer_type,
                buffer, &name, *stride);
        }

        let pv = game.cameras.map_active(|x| {
            x.get_pv_matrix()
        }).unwrap_or(id_matrix());
        let pvm = mul(pv, self.model);

        let iPVM = gl.get_uniform_location(shader_program, "uPVM");
        if iPVM.is_some() {           
            gl.uniform_matrix4fv_with_f32_array(iPVM.as_ref(), false, &pvm);
        }
        let iM = gl.get_uniform_location(shader_program, "uM");
        if iM.is_some() {           
            gl.uniform_matrix4fv_with_f32_array(iM.as_ref(), false, &self.model);
        }

        gl.bind_buffer(WebGlRenderingContext::ELEMENT_ARRAY_BUFFER, 
                self.index_buffer.as_ref());
        let ilen = self.ilen.unwrap();
        gl.draw_elements_with_i32(WebGlRenderingContext::TRIANGLES, 
            ilen, 
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

pub struct CameraMgr
{
    cameras: Vec<Camera>,
    active: Option<usize>,
}

impl CameraMgr
{
    pub fn push(&mut self, cam: Camera, activate: bool) -> usize
    {
        self.cameras.push(cam);
        let i = self.cameras.len() - 1;
        if activate {
            self.set_active(i);
        }
        i
    }

    pub fn set_active(&mut self, i: usize)
    {
        self.active = Some(i);
    }

    pub fn map_active<T>(&self, f: fn(&Camera) -> T) -> Option<T>
    {
        self.active.map(|i| {
            let cam = &self.cameras[i];
            f(cam)
        })
    }

    pub fn do_active<F>(&mut self, f: F)
        where F : Fn(&mut Camera)
    {
        match self.active {
            None => {},
            Some(i) => {
                let cam = &mut self.cameras[i];
                f(cam);
            },
        }
    }
}

pub struct Camera
{
    position: [f32;3],
    at: [f32;3],
    up: [f32;3],
}

impl Camera
{
    pub fn get_pv_matrix(&self) -> [f32;16]
    {
        let p = perspective_matrix(
            std::f32::consts::PI / 4.0,
            640.0 / 480.0, 
            1.0, 5000.0);
        let v = lookat_matrix(
            self.position,
            self.at,
            self.up
        );
        mul(p,v)
    }

    pub fn orbitate(&mut self, scale: f32)
    {
        let f = normalize(sub(self.position, self.at));
        let r = cross(normalize(self.up), f);

        self.position = add(self.position, scale3(r, scale));
    }

    pub fn zoom(&mut self, scale: f32)
    {
        let f = normalize(sub(self.position, self.at));
        self.position = add(self.position, scale3(f, -scale));
    }

    pub fn up_down(&mut self, scale: f32)
    {
        self.position = add(self.position, scale3(self.up, scale));
    }
}

#[wasm_bindgen]
pub struct Game
{
    gl: Rc<WebGlRenderingContext>,
    entities: Vec<Entity>,
    cameras: CameraMgr,
    keys: KeyboardMgr,
    mouse: MouseMgr,
}

#[wasm_bindgen]
pub struct KeyboardMgr
{
    keys: [bool;1024]
}

#[wasm_bindgen]
impl KeyboardMgr
{
    pub fn set_key(&mut self, key: usize, state: bool)
    {
        self.keys[key] = state;
    }

    pub fn map_f32(&self, key1: usize, v1: f32, key2: usize, v2: f32) -> Option<f32>
    {
        if self.keys[key1] { Some(v1) }
        else if self.keys[key2] { Some(v2) }
        else { None }
    }
}

#[wasm_bindgen]
pub struct MouseMgr
{
    keys: [bool;16],
    wheel: f32,
    x: u32,
    y: u32,

    dx: i32,
}

#[wasm_bindgen]
impl MouseMgr
{
    pub fn set_key(&mut self, key: usize, state: bool) { self.keys[key] = state; }
    pub fn set_state(&mut self, x: u32, y: u32, wheel: f32, l: bool, r: bool, m: bool) {
        self.keys[0] = l;
        self.keys[1] = m;
        self.keys[2] = r;
        self.wheel = wheel;
        self.dx = (x as i32) - (self.x as i32);
        self.x = x;
        self.y = y;
    }
    pub fn map_wheel_f32(&self, t: f32, gt: f32, lt: f32) -> Option<f32>
    {
        if self.wheel == t { None }
        else if self.wheel > t { Some(gt) }
        else { Some(lt) }
    }
    pub fn map_dx_f32(&self, t: i32, gt: f32, lt: f32) -> Option<f32>
    {
        if self.dx == t { None }
        else if self.dx > t { Some(gt) }
        else { Some(lt) }
    }
    pub fn is_button(&self, button: usize, state: bool) -> bool
    {
        self.keys[button] == state
    }

    pub fn clear(&mut self) {
        self.keys[0] = false;
        self.keys[1] = false;
        self.keys[2] = false;
        self.wheel = 0.0;

        self.dx = 0;
    }
}

const KEY_PAGEUP: usize = 33;
const KEY_PAGEDOWN: usize = 34;
const KEY_LEFT: usize = 37;
const KEY_UP: usize = 38;
const KEY_RIGHT: usize = 39;
const KEY_DOWN: usize = 40;

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

        let game = Game {
            gl: gl.clone(),
            entities: Vec::new(),
            cameras: CameraMgr {
                cameras: Vec::new(),
                active: None
            },
            keys: KeyboardMgr {
                keys: [false; 1024]
            },
            mouse : MouseMgr {
                keys: [false;16],
                wheel: 0.0,
                x: 0,
                y: 0,
                dx: 0
            }
        };

//         let mut cube = Entity::new();
//         let mut cubegl = WebGlComponent::new(gl.clone());
//         let vertices = vec![
//             -0.5, 0.5, 0.0,
//             -0.5,-0.5, 0.0,
//              0.5,-0.5, 0.0,
//              0.5, 0.5, 0.0
//            ].as_array_buffer();
//         print_array(&vertices);
//         cubegl.push_buffer(&vertices)?;
//         let indices = vec![3,2,1,3,1,0]
//             .as_array_buffer();
//         print_array(&indices);
//         cubegl.push_indices(&indices)?;
//         cubegl.compile_vertex_shader("attribute vec3 coordinates;
// void main(void) {
//     gl_Position = vec4(coordinates, 1.0);
// }")?;
//         cubegl.compile_fragment_shader("
// void main(void) {
//     gl_FragColor = vec4(1.0, 0.0, 0.0, 1);
// }")?;
//         cubegl.link_shaders()?;
//         cube.push_component(cubegl.as_component());
        
        // game.entities.push(cube);

        // message();
        Ok(game)
    }

    pub async fn load(mut self)  -> Result<Game, JsValue>
    {
        let head_vx = fetch_get_octet("data/ty/Boy01_Head_Geo-lib.vertices").await?;
        let head_uv = fetch_get_octet("data/ty/Boy01_Head_Geo-lib.texcoords").await?;
        let head_ix = fetch_get_octet("data/ty/Boy01_Head_Geo-lib.index").await?;
        let head_tex = fetch_get_octet("data/ty/Boy_Head_diffuse.png.tex").await?;

        let mut cube = Entity::new();
        let mut cubegl = WebGlComponent::new(self.gl.clone());
        cubegl.push_buffer("coordinates", 3, &head_vx)?;
        cubegl.push_buffer("texcoords", 2, &head_uv)?;
        cubegl.push_tex("inTex", 1024, &head_tex)?;
        // cubegl.model = transpose([
        //      0.999952, 0.003254,  0.009255, -0.000000,
        //     -0.003123, 0.999895, -0.014167,  8.913815,
        //     -0.009300, 0.014137,  0.999857,  1.034021,
        //      0.000000, 0.000000,  0.000000,  1.000000
        // ]);
        
        cubegl.push_indices(&head_ix)?;
        cubegl.compile_vertex_shader("attribute vec3 coordinates;
attribute vec2 texcoords;
varying highp vec2 outTexCoords;
uniform mat4 uPVM;
void main(void) {
    gl_Position = uPVM * vec4(coordinates, 1.0);
    outTexCoords = texcoords;
}")?;
        cubegl.compile_fragment_shader("
varying highp vec2 outTexCoords;
uniform sampler2D inTex;
void main(void) {
    gl_FragColor = texture2D(inTex, outTexCoords);
    //gl_FragColor = vec4(outTexCoords, 0, 1);
}")?;
        cubegl.link_shaders()?;
        cube.push_component(cubegl.as_component());
        self.entities.push(cube);

        let camera = Camera {
            position: [80.0, 80.0, 100.0],
            at: [0.0, 0.0, 0.0],
            up: [0.0, 1.0, 0.0]
        };
        self.cameras.push(camera, true);

        // GRID

        let mut grid = Entity::new();
        let mut gridgl = WebGlComponent::new(self.gl.clone());
        gridgl.push_buffer("coordinates", 3, &vec![
            -1.0, 0.0, -1.0,
            1.0, 0.0, -1.0,
            1.0, 0.0, 1.0,
            -1.0, 0.0, 1.0,
        ].as_array_buffer())?;
        gridgl.push_indices(&vec![
            0, 1, 2, 0, 2, 3
        ].as_array_buffer())?;
        gridgl.model = transpose([
            100.0, 0.0, 0.0, 0.0,
            0.0, 100.0, 0.0, 0.0,
            0.0, 0.0, 100.0, 0.0,
            0.0, 0.0, 0.0, 1.0
        ]);
        gridgl.compile_vertex_shader("attribute vec3 coordinates;
uniform mat4 uPVM;
uniform mat4 uM;
varying highp vec4 outPos;
void main(void) {
    gl_Position = uPVM * vec4(coordinates, 1.0);
    outPos = (uM * vec4(coordinates, 1.0)) + vec4(1,1,1,0);
    outPos *= 0.5;
}")?;
gridgl.compile_fragment_shader("
varying highp vec4 outPos;
void main(void) {
    gl_FragColor = vec4(outPos.rgb, 1);
}")?;
        gridgl.link_shaders()?;
        grid.push_component(gridgl.as_component());
        self.entities.push(grid);

        Ok(self)
    }

    pub fn set_key(&mut self, key: usize, state: bool)
    {
        self.keys.set_key(key, state);
    }

    pub fn set_mouse(&mut self, 
        x: u32, y: u32, 
        wheel: f32, 
        l: bool, r: bool, m: bool)
    {
        self.mouse.set_state(x, y, wheel, l, r, m);
    }
    
    pub fn update(&mut self, _timestamp: f64)
    {
        let keys = &self.keys;
        let mouse = &mut self.mouse;
        self.cameras.do_active(|x| {
            if let Some(v) = keys.map_f32(KEY_LEFT, -1.0, KEY_RIGHT, 1.0) { x.orbitate(v); }
            if mouse.is_button(0, true) {
                if let Some(v) = mouse.map_dx_f32(0, 10.0, -10.0) { x.orbitate(v); }
            }
            if let Some(v) = keys.map_f32(KEY_UP, 1.0, KEY_DOWN, -1.0) { x.zoom(v); }
            if let Some(v) = mouse.map_wheel_f32(0.0, -10.0, 10.0) { x.zoom(v); }
            if let Some(v) = keys.map_f32(KEY_PAGEUP, 1.0, KEY_PAGEDOWN, -1.0) { x.up_down(v); }
        });

        mouse.clear();
    }

    pub fn render(&self, _timestamp: f64)
    {
        let gl = &self.gl;
        gl.clear_color(0.0, 0.0, 0.0, 1.0);
        gl.clear(WebGlRenderingContext::COLOR_BUFFER_BIT);
        gl.clear(WebGlRenderingContext::DEPTH_BUFFER_BIT);
        gl.viewport(0, 0, 640, 480);
        gl.enable(WebGlRenderingContext::DEPTH_TEST);

        for entity in &self.entities {
            for component in &entity.components {
                match component {
                    Component::WebGL(data) => {
                        data.render(&self);
                    },
                }
            }
        }
    }
}