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
const ARENA_BYTES: usize = 16 * 1024 * 1024;
const LIST_INITIAL_CAPACITY: usize = 1024;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
enum ValueTag {
    Int = 1,
    List = 2,
}

#[repr(C)]
struct Value {
    tag: ValueTag,
    _padding: [u8; 7],
    payload: i64,
}

#[repr(C)]
struct ListHeader {
    ptr: *mut i64,
    len: usize,
    cap: usize,
}

static mut ARENA: [u8; ARENA_BYTES] = [0; ARENA_BYTES];
static mut ARENA_OFFSET: usize = 0;

fn runtime_abort() -> ! {
    unsafe {
        ExitProcess(1);
    }
}

fn align_up(value: usize, align: usize) -> usize {
    (value + (align - 1)) & !(align - 1)
}

unsafe fn arena_alloc(size: usize, align: usize) -> *mut u8 {
    let base = core::ptr::addr_of_mut!(ARENA) as *mut u8 as usize;
    let start = align_up(base + ARENA_OFFSET, align) - base;
    let end = match start.checked_add(size) {
        Some(v) => v,
        None => runtime_abort(),
    };
    if end > ARENA_BYTES {
        runtime_abort();
    }
    ARENA_OFFSET = end;
    core::ptr::addr_of_mut!(ARENA).cast::<u8>().add(start)
}

fn value_ptr(handle: i64) -> *mut Value {
    if handle == 0 {
        runtime_abort();
    }
    handle as usize as *mut Value
}

unsafe fn alloc_value(tag: ValueTag, payload: i64) -> i64 {
    let ptr = arena_alloc(core::mem::size_of::<Value>(), core::mem::align_of::<Value>()) as *mut Value;
    (*ptr).tag = tag;
    (*ptr)._padding = [0; 7];
    (*ptr).payload = payload;
    ptr as usize as i64
}

fn as_int(handle: i64) -> i64 {
    unsafe {
        let ptr = value_ptr(handle);
        if (*ptr).tag != ValueTag::Int {
            runtime_abort();
        }
        (*ptr).payload
    }
}

fn as_list_header_ptr(handle: i64) -> *mut ListHeader {
    unsafe {
        let ptr = value_ptr(handle);
        if (*ptr).tag != ValueTag::List {
            runtime_abort();
        }
        (*ptr).payload as usize as *mut ListHeader
    }
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

fn write_i64(n: i64) {
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

    let mut out = [0u8; 21];
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
    write_stdout(&out[..idx]);
}

fn print_value_inner(handle: i64) {
    unsafe {
        let ptr = value_ptr(handle);
        match (*ptr).tag {
            ValueTag::Int => write_i64((*ptr).payload),
            ValueTag::List => {
                let header = &*((*ptr).payload as usize as *const ListHeader);
                write_stdout(b"[");
                let mut i = 0usize;
                while i < header.len {
                    if i != 0 {
                        write_stdout(b", ");
                    }
                    print_value_inner(*header.ptr.add(i));
                    i += 1;
                }
                write_stdout(b"]");
            }
        }
    }
}

fn new_int(value: i64) -> i64 {
    unsafe { alloc_value(ValueTag::Int, value) }
}

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

#[unsafe(no_mangle)]
pub extern "C" fn __expr_value_int_host(raw: i64) -> i64 {
    new_int(raw)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    write_stdout(b"\n");
    new_int(0)
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_new_host() -> i64 {
    unsafe {
        let data_ptr = arena_alloc(
            LIST_INITIAL_CAPACITY * core::mem::size_of::<i64>(),
            core::mem::align_of::<i64>(),
        ) as *mut i64;
        let header_ptr = arena_alloc(
            core::mem::size_of::<ListHeader>(),
            core::mem::align_of::<ListHeader>(),
        ) as *mut ListHeader;
        (*header_ptr).ptr = data_ptr;
        (*header_ptr).len = 0;
        (*header_ptr).cap = LIST_INITIAL_CAPACITY;
        alloc_value(ValueTag::List, header_ptr as usize as i64)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_push_host(handle: i64, value: i64) -> i64 {
    unsafe {
        let header = &mut *as_list_header_ptr(handle);
        if header.len == header.cap {
            let new_cap = match header.cap.checked_mul(2) {
                Some(v) => v,
                None => runtime_abort(),
            };
            let new_ptr = arena_alloc(
                new_cap * core::mem::size_of::<i64>(),
                core::mem::align_of::<i64>(),
            ) as *mut i64;
            core::ptr::copy_nonoverlapping(header.ptr, new_ptr, header.len);
            header.ptr = new_ptr;
            header.cap = new_cap;
        }
        *header.ptr.add(header.len) = value;
        header.len += 1;
        handle
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_insert_host(handle: i64, index: i64, value: i64) -> i64 {
    unsafe {
        let header = &*as_list_header_ptr(handle);
        let idx_raw = as_int(index);
        if idx_raw < 0 {
            runtime_abort();
        }
        let idx = idx_raw as usize;
        if idx > header.len {
            runtime_abort();
        }
        if header.len == header.cap {
            let new_cap = match header.cap.checked_mul(2) {
                Some(v) => v,
                None => runtime_abort(),
            };
            let new_ptr = arena_alloc(
                new_cap * core::mem::size_of::<i64>(),
                core::mem::align_of::<i64>(),
            ) as *mut i64;
            core::ptr::copy_nonoverlapping(header.ptr, new_ptr, header.len);
            let header_mut = &mut *as_list_header_ptr(handle);
            header_mut.ptr = new_ptr;
            header_mut.cap = new_cap;
        }
        let header = &mut *as_list_header_ptr(handle);
        let mut pos = header.len;
        while pos > idx {
            *header.ptr.add(pos) = *header.ptr.add(pos - 1);
            pos -= 1;
        }
        *header.ptr.add(idx) = value;
        header.len += 1;
        handle
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_len_host(handle: i64) -> i64 {
    unsafe {
        let header = &*as_list_header_ptr(handle);
        new_int(header.len as i64)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_get_host(handle: i64, index: i64) -> i64 {
    unsafe {
        let header = &*as_list_header_ptr(handle);
        let idx_raw = as_int(index);
        if idx_raw < 0 {
            runtime_abort();
        }
        let idx = idx_raw as usize;
        if idx >= header.len {
            runtime_abort();
        }
        *header.ptr.add(idx)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_set_host(handle: i64, index: i64, value: i64) -> i64 {
    unsafe {
        let header = &mut *as_list_header_ptr(handle);
        let idx_raw = as_int(index);
        if idx_raw < 0 {
            runtime_abort();
        }
        let idx = idx_raw as usize;
        if idx >= header.len {
            runtime_abort();
        }
        *header.ptr.add(idx) = value;
        value
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_swap_host(handle: i64, index_a: i64, index_b: i64) -> i64 {
    unsafe {
        let header = &mut *as_list_header_ptr(handle);
        let idx_a_raw = as_int(index_a);
        let idx_b_raw = as_int(index_b);
        if idx_a_raw < 0 || idx_b_raw < 0 {
            runtime_abort();
        }
        let idx_a = idx_a_raw as usize;
        let idx_b = idx_b_raw as usize;
        if idx_a >= header.len || idx_b >= header.len {
            runtime_abort();
        }
        core::ptr::swap(header.ptr.add(idx_a), header.ptr.add(idx_b));
        handle
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_pop_host(handle: i64) -> i64 {
    unsafe {
        let header = &mut *as_list_header_ptr(handle);
        if header.len == 0 {
            runtime_abort();
        }
        header.len -= 1;
        *header.ptr.add(header.len)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_copy_host(handle: i64) -> i64 {
    unsafe {
        let src = &*as_list_header_ptr(handle);
        let cap = if src.cap < LIST_INITIAL_CAPACITY {
            LIST_INITIAL_CAPACITY
        } else {
            src.cap
        };
        let data_ptr = arena_alloc(
            cap * core::mem::size_of::<i64>(),
            core::mem::align_of::<i64>(),
        ) as *mut i64;
        if src.len > 0 {
            core::ptr::copy_nonoverlapping(src.ptr, data_ptr, src.len);
        }
        let header_ptr = arena_alloc(
            core::mem::size_of::<ListHeader>(),
            core::mem::align_of::<ListHeader>(),
        ) as *mut ListHeader;
        (*header_ptr).ptr = data_ptr;
        (*header_ptr).len = src.len;
        (*header_ptr).cap = cap;
        alloc_value(ValueTag::List, header_ptr as usize as i64)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn __expr_list_print_host(handle: i64) -> i64 {
    print_value_inner(handle);
    write_stdout(b"\n");
    new_int(0)
}

unsafe extern "C" {
    fn expr_main_entry() -> i64;
}

#[no_mangle]
pub extern "C" fn mainCRTStartup() -> ! {
    let code = unsafe { expr_main_entry() };
    let int_code = as_int(code);
    let exit_code = if int_code < u32::MIN as i64 || int_code > u32::MAX as i64 {
        1
    } else {
        int_code as u32
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
