
pub mod regs;
pub mod cpu;
pub mod mem;
pub mod inst;

pub mod mcu;

fn main() {
    use std::io::Read;
    let mut cpu = cpu::Cpu::new::<mcu::atmega328p::Mcu>();

    let mut program_file = std::fs::File::open("/home/dylan/avr.bin").unwrap();
    let program_bytes = program_file.bytes().map(|a| a.unwrap());
    cpu.load_program_space(program_bytes);

    println!("Register state: {:?}", cpu.register_file());

    for _ in 0..20 {
        cpu.tick();

        println!("Register state: {:?}", cpu.register_file());
    }

}
