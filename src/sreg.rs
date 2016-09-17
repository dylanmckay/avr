use regs::Register;

pub const CARRY_FLAG: u8 = (1<<0);
pub const ZERO_FLAG: u8 = (1<<1);
pub const NEGATIVE_FLAG: u8 = (1<<2);
pub const OVERFLOW_FLAG: u8 = (1<<3);
pub const S_FLAG: u8 = (1<<4);
pub const HALF_CARRY_FLAG: u8 = (1<<5);
pub const TRANSFER_FLAG: u8 = (1<<6);
pub const INTERRUPT_FLAG: u8 = (1<<7);

/// The AVR status register.
#[derive(Clone,Debug,PartialEq,Eq)]
pub struct SReg(pub Register);

impl SReg
{
    pub fn new() -> Self {
        SReg(Register {
            name: "SREG".into(),
            value: 0,
        })
    }


    pub fn set(&mut self, flag: u8, state: bool) {
        // TODO: update S flag. should be `N xor V`.

        if state == true { self.0.value |= flag } else { self.0.value &= !flag };
    }

    pub fn get(&self, flag: u8) -> bool {
        (self.0.value & flag) == flag
    }

    pub fn is_set(&self, flag: u8) -> bool { self.get(flag) }
    pub fn is_clear(&self, flag: u8) -> bool { !self.get(flag) }
}

