
mod hallo;

fn main() {
    println!("hello from rust, {} {}", unsafe {hallo::add(3, 774484)}, unsafe {hallo::subtract(4, 123)});
}
