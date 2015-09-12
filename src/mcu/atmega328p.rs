
use mcu;

pub struct Mcu;

impl mcu::Mcu for Mcu
{
    fn flash_size() -> usize {
        32 * 1024 // 32 KB
    }

    fn sram_size() -> usize {
        2 * 1024 // 2KB
    }
}
