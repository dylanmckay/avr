
pub mod atmega328p;

use regs::RegisterFile;

/// A microcontroller.
pub trait Chip
{
    fn register_file() -> RegisterFile {
        let mut file = Vec::new();

        // Create GPRs (r0-r31).
        for _ in 0..32 {
            file.push(0);
        }

        let sram_end = Self::sram_size()-1;
        let sram_size_lo = sram_end & 0x00ff;
        let sram_size_hi = (sram_end & 0xff00) >> 8;

        // Innitialize SP
        file.push(sram_size_lo as u8);
        file.push(sram_size_hi as u8);

        RegisterFile::new(file)
    }

    fn flash_size() -> usize;
    fn sram_size() -> usize;
}
