

// TODO: s/addr/num

use std::collections::HashMap;

/// A register value.
pub type Register = u8;

pub const CARRY_FLAG: u8 = (1<<0);
pub const ZERO_FLAG: u8 = (1<<1);
pub const NEGATIVE_FLAG: u8 = (1<<2);
pub const OVERFLOW_FLAG: u8 = (1<<3);
pub const S_FLAG: u8 = (1<<4);
pub const HALF_CARRY_FLAG: u8 = (1<<5);
pub const TRANSFER_FLAG: u8 = (1<<6);
pub const INTERRUPT_FLAG: u8 = (1<<7);

/// `SP` low register number.
pub const SP_LO_NUM: u8 = 32;
/// `SP` high register number.
pub const SP_HI_NUM: u8 = 33;

/// The register file.
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct RegisterFile
{
    registers: Vec<Register>,
    sreg: Register,
}

impl RegisterFile
{
    pub fn new(registers: Vec<Register>) -> Self {
        RegisterFile {
            registers: registers,
            sreg: 0,
        }
    }

    /// Gets a register, or `None` if it doesn't exist.
    pub fn gpr(&self, addr: u8)
        -> Option<&Register> {

        self.registers.get(addr as usize)
    }

    /// Gets a mutable register, or `None` if it doesn't exist.
    pub fn gpr_mut(&mut self, addr: u8)
        -> Option<&mut Register> {

        self.registers.get_mut(addr as usize)
    }

    pub fn gpr_val(&self, addr: u8)
        -> Option<Register> {
        self.gpr(addr).map(|&a| a)
    }

    pub fn gpr_pair(&self, addr: u8)
        -> Option<(&Register,&Register)> {
        assert!(addr % 2 == 0,
                "GPR pairs must be even");

        let lo = self.gpr(addr).unwrap();
        let hi = self.gpr(addr+1).unwrap();

        Some((lo, hi))
    }

    pub fn gpr_pair_val(&self, addr: u8)
        -> Option<u16> {
        let (lo,hi) = self.gpr_pair(addr).unwrap();
        let val = ((*hi as u16) << 8) | *lo as u16;
        Some(val)
    }

    pub fn set_gpr_pair(&mut self, low: u8, val: u16) {
        let val_lo = ((val & 0x00ff) >> 0) as u8;
        let val_hi = ((val & 0xff00) >> 8) as u8;

        *self.gpr_mut(low).unwrap() = val_lo;
        *self.gpr_mut(low+1).unwrap() = val_hi;
    }

    pub fn sreg(&self) -> &Register { &self.sreg }
    pub fn sreg_mut(&mut self) -> &mut Register { &mut self.sreg }

    /// Checks if a flag is set in SREG.
    pub fn sreg_flag(&self, mask: u8) -> bool {
        (self.sreg & mask) == mask
    }

    pub fn sreg_flag_set(&mut self, mask: u8) {
        self.sreg |= mask;
    }

    pub fn sreg_flag_clear(&mut self, mask: u8) {
        self.sreg &= !mask;
    }
}
