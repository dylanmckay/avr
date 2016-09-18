#![feature(question_mark)]

#![feature(plugin)]
#![cfg_attr(test, plugin(stainless))]

pub use self::core::Core;
pub use self::mcu::Mcu;
pub use self::regs::{Register,RegisterFile};
pub use self::mem::Space;
pub use self::inst::Instruction;
pub use self::addons::Addon;
pub use self::error::Error;
pub use self::sreg::SReg;
pub use self::program::Program;

pub mod core;
pub mod mcu;
pub mod regs;
pub mod mem;
pub mod inst;
pub mod io;
pub mod error;
pub mod sreg;
pub mod math;
pub mod program;

pub mod addons;
pub mod chips;

#[macro_use]
extern crate nom;

