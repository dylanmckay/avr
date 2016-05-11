pub use self::uart::Uart;

pub mod uart;

trait Addon
{
    fn tick(&mut self, mcu: &mut ::Mcu);
}
