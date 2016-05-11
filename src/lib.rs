pub use self::core::Core;
pub use self::regs::{Register,RegisterFile};
pub use self::mem::Space;
pub use self::inst::Instruction;

pub mod regs;
pub mod core;
pub mod mem;
pub mod inst;
pub mod addons;

pub mod chips;

// fn main() {
//     use std::io::Read;
//     let mut core = core::Core::new::<chips::atmega328p::Chip>();
//
//     let mut args = std::env::args();
//     args.next(); // eat the program name.
//     let program_path = args.next().expect("expected a '.bin' program path");
//
//     let program_file = std::fs::File::open(program_path).unwrap();
//     let program_bytes = program_file.bytes().map(|a| a.unwrap());
//     core.load_program_space(program_bytes);
//
//     for _ in 0..20 {
//         core.tick();
//     }
// }
