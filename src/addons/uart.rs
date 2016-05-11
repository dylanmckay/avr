use Core;

pub struct Uart
{
    baud: u64,
}

impl Uart
{
    fn tick(&mut self, core: &mut Core) {
        println!("tick");
    }
}
