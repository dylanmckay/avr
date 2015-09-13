
use chips;

pub struct Chip;

impl chips::Chip for Chip
{
    fn flash_size() -> usize {
        32 * 1024 // 32 KB
    }

    fn sram_size() -> usize {
        2 * 1024 // 2KB
    }
}
