mod counter;
use runtime::*;

static mut APPS: Vec<Applications> = Vec::new();

enum Applications {
    Login(counter::App),
}

impl Applications {
    pub fn send_by_id(&mut self, id: usize, p: Vec<u64>) -> std::result::Result<(), u64> {
        match self {
            Applications::Login(app) => app.send_by_id(id, p),
        }
    }

    pub fn render(&mut self) -> Html {
        match self {
            Applications::Login(app) => app.render(),
        }
    }
}

#[no_mangle]
pub fn mount() -> u64 {
    std::panic::set_hook(Box::new(|panic_info| {
        let msg = format!("{}", panic_info);
        unsafe { console_error(msg.len() as u32, msg.as_ptr() as u32) };
    }));

    let app = counter::App::new(counter::init, counter::update, counter::view);

    unsafe {
        APPS.push(Applications::Login(app));
        let id = APPS.len() as u64 - 1;

        match APPS.get_mut(id as usize) {
            Some(app) => {
                app.render();
            }
            None => {}
        }

        id
    }
}

#[no_mangle]
pub fn send(app_idx: u64, msg_idx: u64, p0: u64, p1: u64, p2: u64, p3: u64, p4: u64) -> bool {
    let p = vec![p0, p1, p2, p3, p4];
    unsafe {
        if let Some(app) = APPS.get_mut(app_idx as usize) {
            match app.send_by_id(msg_idx as usize, p) {
                Ok(_) => true,
                Err(_) => false,
            }
        } else {
            false
        }
    }
}
