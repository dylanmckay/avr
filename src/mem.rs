use Error;
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

    pub fn set_u8(&mut self, addr: usize, val: u8) -> Result<(), Error> {
        if self.is_access_in_bounds(addr, 1) {
            self.data[addr] = val;
            Ok(())
        } else {
            Err(Error::SegmentationFault { address: addr + 1 })
        }
    }

    pub fn set_u16(&mut self, addr: usize, val: u16) -> Result<(), Error> {
        if self.is_access_in_bounds(addr, 2) {
            self.data[addr] = ((val & 0xff00) >> 8) as u8;
            self.data[addr+1] = (val & 0xff) as u8;
            Ok(())
        } else {
            Err(Error::SegmentationFault { address: addr + 2 })
        }
    }

    pub fn get_u8(&self, addr: usize) -> Result<u8, Error> {
        self.data.get(addr).cloned().ok_or(Error::SegmentationFault { address: addr })
    }

    pub fn get_u16(&self, addr: usize) -> Result<u16, Error> {
        let hi = self.get_u8(addr+0)? as u16;
        let lo = self.get_u8(addr+1)? as u16;

        Ok((hi << 8) | lo)
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

    fn is_access_in_bounds(&self, addr: usize, byte_count: usize) -> bool {
        let end_byte_offset = addr + byte_count;
        end_byte_offset <= self.data.len()
    }
}
