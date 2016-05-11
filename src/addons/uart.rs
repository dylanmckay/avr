use Mcu;

pub struct Uart
{
    baud: u64,
}

impl Uart
{
    fn tick(&mut self, mcu: &mut Mcu) {
        println!("tick");
    }
}
