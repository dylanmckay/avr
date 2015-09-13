
pub mod regs;
pub mod mcu;
pub mod mem;
pub mod inst;

pub mod chips;

fn main() {
    use std::io::Read;
    let mut mcu = mcu::Mcu::new::<chips::atmega328p::Chip>();

    let program_file = std::fs::File::open("/home/dylan/avr.bin").unwrap();
    let program_bytes = program_file.bytes().map(|a| a.unwrap());
    mcu.load_program_space(program_bytes);

    for _ in 0..20 {
        mcu.tick();
    }

}
