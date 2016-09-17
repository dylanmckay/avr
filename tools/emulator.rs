extern crate avr;

fn main() {
    use std::io::Read;

    let mut core = avr::Core::new::<avr::chips::atmega328p::Chip>();

    let mut args = std::env::args();
    args.next(); // eat the program name.
    let program_path = args.next().expect("expected a '.bin' program path");

    let program_file = std::fs::File::open(program_path).unwrap();
    let program_bytes = program_file.bytes().map(|a| a.unwrap());
    core.load_program_space(program_bytes);

    let mut mcu = avr::Mcu::new(core);

    let uart = avr::addons::Uart::new(
        16000000, // CPU frequency
        187000,   // Baud rate
        avr::io::Port::new(0x24), // Tx
        avr::io::Port::new(0x25), // Rx
    );

    mcu.attach(Box::new(uart));

    for _ in 0..70 {
        mcu.tick().expect("failed while ticking");
    }
}
