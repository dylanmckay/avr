pub use self::uart::Uart;

pub mod uart;

pub trait Addon
{
    fn tick(&mut self, core: &mut ::Core);
}
