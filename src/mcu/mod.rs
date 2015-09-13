
pub mod atmega328p;

use regs::RegisterFile;
use std::collections::HashMap;

/// A microcontroller.
pub trait Mcu
{
    fn register_file() -> RegisterFile {
        let mut file = HashMap::new();

        for gpr in 0..31 {
            file.insert(gpr, 0);
        }

        let sram_end = Self::sram_size()-1;
        let sram_size_lo = sram_end & 0x00ff;
        let sram_size_hi = (sram_end & 0xff00) >> 8;

        // Innitialize SP
        file.insert(32, sram_size_lo as u8);
        file.insert(33, sram_size_hi as u8);

        RegisterFile::new(file)
    }

    fn flash_size() -> usize;
    fn sram_size() -> usize;
}
