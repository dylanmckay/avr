
pub mod atmega328p;

use regs::RegisterFile;
use std::collections::HashMap;

/// A microcontroller.
pub trait Mcu
{
    fn register_file() -> RegisterFile {
        let mut file = HashMap::new();

        for gpr in 0..33 {
            file.insert(gpr, 0);
        }

        RegisterFile::new(file)
    }

    fn flash_size() -> usize;
    fn sram_size() -> usize;
}
