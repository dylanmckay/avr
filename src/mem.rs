
use std;

pub type Address = u16;

/// A memory space.
pub struct Space
{
    data: Vec<u8>,
}

impl Space
{
    pub fn new(size: usize) -> Self {
        let data = std::iter::repeat(0).take(size)
                                       .collect();
        Space {
            data: data,
        }
    }

    pub fn set_u8(&mut self, addr: usize, val: u8) {
        self.data[addr] = val;
    }

    pub fn set_u16(&mut self, addr: usize, val: u16) {
        self.data[addr] = ((val & 0xff00) >> 8) as u8;
        self.data[addr+1] = (val & 0xff) as u8;
    }

    pub fn get_u8(&self, addr: usize) -> u8 {
        self.data[addr]
    }

    pub fn get_u16(&self, addr: usize) -> u16 {
        let hi = self.data[addr] as u16;
        let lo = self.data[addr+1] as u16;

        (hi << 8) | lo
    }

    pub fn bytes<'a>(&'a self) -> std::slice::Iter<'a,u8> {
        self.data.iter()
    }

    pub fn bytes_mut<'a>(&'a mut self) -> std::slice::IterMut<'a,u8> {
        self.data.iter_mut()
    }

    pub fn load<I>(&mut self, mut bytes: I)
        where I: Iterator<Item=u8> {

        for byte in self.data.iter_mut() {
            if let Some(b) = bytes.next() {
                *byte = b;
            } else {
                 break;
            }
        }
    }
}
