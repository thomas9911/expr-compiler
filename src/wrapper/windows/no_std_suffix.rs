#[no_mangle]
pub extern "C" fn mainCRTStartup() -> ! {
    unsafe { ExitProcess(expr_windows_main()); }
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    unsafe {
        ExitProcess(1);
    }
}
