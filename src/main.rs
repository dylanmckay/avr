
pub mod regs;
pub mod cpu;
pub mod mem;
pub mod inst;

pub mod mcu;

fn main() {
    use std::io::Read;
    let mut cpu = cpu::Cpu::new::<mcu::atmega328p::Mcu>();

    let program_file = std::fs::File::open("/home/dylan/avr.bin").unwrap();
    let program_bytes = program_file.bytes().map(|a| a.unwrap());
    cpu.load_program_space(program_bytes);

    for _ in 0..20 {
        cpu.tick();
    }

}
