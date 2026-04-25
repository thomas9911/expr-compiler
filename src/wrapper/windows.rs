#![no_std]
#![no_main]

use core::panic::PanicInfo;
use core::ptr;

#[link(name = "kernel32")]
extern "system" {
    fn GetStdHandle(nStdHandle: i32) -> *mut core::ffi::c_void;
    fn WriteFile(
        hFile: *mut core::ffi::c_void,
        lpBuffer: *const u8,
        nNumberOfBytesToWrite: u32,
        lpNumberOfBytesWritten: *mut u32,
        lpOverlapped: *mut core::ffi::c_void,
    ) -> i32;
    fn ExitProcess(uExitCode: u32) -> !;
}

const STD_OUTPUT_HANDLE: i32 = -11;

#[unsafe(no_mangle)]
pub extern "C" fn memcpy(dst: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    let mut i = 0usize;
    unsafe {
        while i < n {
            *dst.add(i) = *src.add(i);
            i += 1;
        }
    }
    dst
}

#[unsafe(no_mangle)]
pub extern "C" fn memset(dst: *mut u8, value: i32, n: usize) -> *mut u8 {
    let mut i = 0usize;
    let byte = value as u8;
    unsafe {
        while i < n {
            *dst.add(i) = byte;
            i += 1;
        }
    }
    dst
}

#[unsafe(no_mangle)]
pub extern "C" fn memcmp(a: *const u8, b: *const u8, n: usize) -> i32 {
    let mut i = 0usize;
    unsafe {
        while i < n {
            let x = *a.add(i);
            let y = *b.add(i);
            if x != y {
                return x as i32 - y as i32;
            }
            i += 1;
        }
    }
    0
}

#[unsafe(no_mangle)]
pub extern "C" fn __CxxFrameHandler3() -> i32 {
    0
}

fn write_stdout(buf: &[u8]) {
    unsafe {
        let handle = GetStdHandle(STD_OUTPUT_HANDLE);
        if handle.is_null() {
            return;
        }
        let mut written: u32 = 0;
        let _ = WriteFile(
            handle,
            buf.as_ptr(),
            buf.len() as u32,
            &mut written as *mut u32,
            ptr::null_mut(),
        );
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_print_host(n: i64) -> i64 {
    let negative = n < 0;
    let mut value = if negative {
        (-(n as i128)) as u128
    } else {
        n as u128
    };

    let mut rev_digits = [0u8; 20];
    let mut rev_len = 0usize;
    loop {
        rev_digits[rev_len] = b'0' + (value % 10) as u8;
        rev_len += 1;
        value /= 10;
        if value == 0 {
            break;
        }
    }

    let mut out = [0u8; 22];
    let mut idx = 0usize;
    if negative {
        out[idx] = b'-';
        idx += 1;
    }
    let mut i = rev_len;
    while i > 0 {
        i -= 1;
        out[idx] = rev_digits[i];
        idx += 1;
    }
    out[idx] = b'\n';
    idx += 1;

    write_stdout(&out[..idx]);
    0
}

unsafe extern "C" {
    fn expr_main_entry() -> i64;
}

#[no_mangle]
pub extern "C" fn mainCRTStartup() -> ! {
    let code = unsafe { expr_main_entry() };
    let exit_code = if code < u32::MIN as i64 || code > u32::MAX as i64 {
        1
    } else {
        code as u32
    };

    unsafe {
        ExitProcess(exit_code);
    }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    unsafe {
        ExitProcess(1);
    }
}
