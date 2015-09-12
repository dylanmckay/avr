
pub mod atmega328p;

use regs::RegisterFile;

/// A microcontroller.
pub trait Mcu
{
    fn register_file() -> RegisterFile;

    fn flash_size() -> usize;
    fn sram_size() -> usize;
}
