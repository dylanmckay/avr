use chips;
use io;

pub struct Chip;

impl chips::Chip for Chip
{
    fn flash_size() -> usize {
        32 * 1024 // 32 KB
    }

    fn memory_size() -> usize {
        2 * 1024 // 2KB
    }

    fn io_ports() -> Vec<io::Port> {
        vec![
            io::Port::new(0x03), // PINB
            io::Port::new(0x04), // DDRB
            io::Port::new(0x05), // PORTB

            io::Port::new(0x06), // PINC
            io::Port::new(0x07), // DDRC
            io::Port::new(0x08), // PORTC

            io::Port::new(0x09), // PIND
            io::Port::new(0x0a), // DDRD
            io::Port::new(0x0b), // PORTD
        ]
    }
}
