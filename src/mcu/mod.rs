
pub mod atmega328p;

use regs::RegisterFile;

/// A microcontroller.
pub trait Mcu
{
    fn register_file() -> RegisterFile;
    fn sram_size() -> usize;
}
