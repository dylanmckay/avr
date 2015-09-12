
use std::collections::HashMap;

/// A register value.
pub type Register = u8;

pub const CARRY_BIT: u8 = 0;
pub const ZERO_BIT: u8 = 1;
pub const NEGATIVE_BIT: u8 = 2;
pub const OVERFLOW_BIT: u8 = 3;
pub const S_BIT: u8 = 4;
pub const HALF_CARRY_BIT: u8 = 5;
pub const TRANSFER_BIT: u8 = 6;
pub const INTERRUPT_BIT: u8 = 7;

pub const CARRY_MASK: u8 = (1<<CARRY_BIT);
pub const ZERO_MASK: u8 = (1<<ZERO_BIT);
pub const NEGATIVE_MASK: u8 = (1<<NEGATIVE_BIT);
pub const OVERFLOW_MASK: u8 = (1<<OVERFLOW_BIT);
pub const S_MASK: u8 = (1<<S_BIT);
pub const HALF_CARRY_MASK: u8 = (1<<HALF_CARRY_BIT);
pub const TRANSFER_MASK: u8 = (1<<TRANSFER_BIT);
pub const INTERRUPT_MASK: u8 = (1<<INTERRUPT_BIT);


/// The register file.
#[derive(Clone,Debug)]
pub struct RegisterFile
{
    registers: HashMap<u8, Register>,
    sreg: Register,
}

impl RegisterFile
{
    pub fn new(registers: HashMap<u8,Register>) -> Self {
        RegisterFile {
            registers: registers,
            sreg: 0,
        }
    }

    /// Gets a register, or `None` if it doesn't exist.
    pub fn gpr(&self, addr: u8)
        -> Option<&Register> {

        self.registers.get(&addr)
    }

    /// Gets a mutable register, or `None` if it doesn't exist.
    pub fn gpr_mut(&mut self, addr: u8)
        -> Option<&mut Register> {

        self.registers.get_mut(&addr)
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

    pub fn sreg(&self) -> &Register { &self.sreg }
    pub fn sreg_mut(&mut self) -> &mut Register { &mut self.sreg }

    /// Checks if a flag is set in SREG.
    pub fn sreg_flag(&self, mask: u8) -> bool {
        (self.sreg & mask) == mask
    }
}
