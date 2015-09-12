
use mcu;
use regs::RegisterFile;

use std::collections::HashMap;

pub struct Mcu;

impl mcu::Mcu for Mcu
{
    fn register_file() -> RegisterFile {
        let mut file = HashMap::new();

        for gpr in 0..31 {
            file.insert(gpr, 0);
        }

        RegisterFile::new(file)
    }

    fn sram_size() -> usize {
        2 * 1024 // 2KB
    }
}
