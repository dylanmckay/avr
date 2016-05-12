use Core;
use Addon;
use io;

pub struct Uart
{
    /// The baud rate (bits/second)
    baud: u64,
    /// The number of CPU ticks in a single second (ticks/second)
    cpu_frequency: u64,
    /// Number of ticks between each bit.
    ticks_between_bits: u64,

    ticks_until_next_bit: u64,

    tx: io::Port,
    rx: io::Port,

    processed_bits: Vec<u8>,
}

impl Uart
{
    pub fn new(cpu_frequency: u64, baud: u64, tx: io::Port, rx: io::Port) -> Self {
        let ticks_between_bits = cpu_frequency / baud;

        Uart {
            cpu_frequency: cpu_frequency,
            baud: baud,
            tx: tx,
            rx: rx,

            ticks_between_bits: ticks_between_bits, // TODO: set this variable
            ticks_until_next_bit: ticks_between_bits,

            processed_bits: Vec::new(),
        }
    }

    fn process_bit(&mut self, core: &mut Core) {
        println!("tick");
    }
}

impl Addon for Uart
{
    fn tick(&mut self, core: &mut Core) {
        self.ticks_until_next_bit -= 1;

        if self.ticks_until_next_bit == 0 {
            self.process_bit(core);
            self.ticks_until_next_bit = self.ticks_between_bits;
        }
    }
}

