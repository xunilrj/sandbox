#![windows_subsystem = "windows"]
extern crate winapi;

use std::ffi::OsStr;
use std::os::windows::ffi::OsStrExt;
use std::iter::once;
use std::mem;
use std::ptr::null_mut;
use std::io::Error;

use self::winapi::um::winuser::{
    DefWindowProcW,
    RegisterClassW,
    CreateWindowExW,
    TranslateMessage,
    DispatchMessageW,
    GetMessageW,
};

use self::winapi::shared::windef::*;
use self::winapi::um::libloaderapi::GetModuleHandleW;

use self::winapi::um::winuser::{
    MSG,
    WNDCLASSW,
    CS_OWNDC,
    CS_HREDRAW,
    CS_VREDRAW,
    CW_USEDEFAULT,
    WS_OVERLAPPEDWINDOW,
    WS_VISIBLE,
};

fn win32_string( value : &str ) -> Vec<u16> {
    OsStr::new( value ).encode_wide().chain( once( 0 ) ).collect()
}

struct Window {
    handle : HWND,
}

fn create_window( name : &str, title : &str ) -> Result<Window, Error> {
    let name = win32_string( name );
    let title = win32_string( title );

    unsafe {
        let hinstance = GetModuleHandleW( null_mut() );
        let wnd_class = WNDCLASSW {
            style : CS_OWNDC | CS_HREDRAW | CS_VREDRAW,
            lpfnWndProc : Some( DefWindowProcW ),
            hInstance : hinstance,
            lpszClassName : name.as_ptr(),
            cbClsExtra : 0,
            cbWndExtra : 0,
            hIcon: null_mut(),
            hCursor: null_mut(),
            hbrBackground: null_mut(),
            lpszMenuName: null_mut(),
        };

        RegisterClassW( &wnd_class );

        let handle = CreateWindowExW(
            0,
            name.as_ptr(),
            title.as_ptr(),
            WS_OVERLAPPEDWINDOW | WS_VISIBLE,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            null_mut(),
            null_mut(),
            hinstance,
            null_mut() );

        if handle.is_null() {
            Err( Error::last_os_error() )
        } else {
            Ok( Window { handle } )
        }
    }
}

fn handle_message( window : &mut Window ) -> bool {
    unsafe {
        let mut message : MSG = mem::uninitialized();
        if GetMessageW( &mut message as *mut MSG, window.handle, 0, 0 ) > 0 {
            TranslateMessage( &message as *const MSG );
            DispatchMessageW( &message as *const MSG );

            true
        } else {
            false
        }
    }
}

fn main() {
    let mut window = create_window( "my_window", "Hello Windows" ).unwrap();

    loop {
        if !handle_message( &mut window ) {
            break;
        }
    }
}