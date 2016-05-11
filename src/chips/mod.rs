pub mod atmega328p;

use regs::{RegisterFile,Register};
use io;

/// A microcontroller.
pub trait Chip
{
    fn register_file() -> RegisterFile {
        let mut file = Vec::new();

        // Create GPRs (r0-r31).
        for number in 0..32 {
            file.push(Register {
                name: format!("r{}", number),
                value: 0,
            });
        }

        let memory_end = Self::memory_size()-1;
        let memory_size_lo = memory_end & 0x00ff;
        let memory_size_hi = (memory_end & 0xff00) >> 8;

        // Innitialize SP
        file.push(Register {
            name: "SPH".into(),
            value: memory_size_hi as u8,
        });

        file.push(Register {
            name: "SPL".into(),
            value: memory_size_lo as u8,
        });

        RegisterFile::new(file)
    }

    fn io_ports() -> Vec<io::Port>;

    fn flash_size() -> usize;
    fn memory_size() -> usize;
}
