
pub mod regs;
pub mod cpu;
pub mod mem;
pub mod inst;

pub mod mcu;

use std::fs;

fn main() {
    let cpu = cpu::Cpu::new::<mcu::atmega328p::Mcu>();

    let mut file = fs::File::open("/home/dylan/avr.bin").unwrap();

    let inst = inst::Instruction::read(&mut file);
    println!("instruction: {:?}", inst);

}
