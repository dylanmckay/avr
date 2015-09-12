
/// An instruction.
#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum Instruction
{
    Rd(OpRd, u8),
    RdK(OpRdK, u8, u8),
    RdRr(OpRdRr, u8, u8),
    /// Instructions with no arguments.
    /// TODO: give better name.
    N(OpN),
    K(OpK, u32),
}

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum OpRd
{
    Inc,
    Dec,
    Com,
    Neg,
    Push,
    Pop,
    Swap,
}

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum OpRdK
{
    Subi,
    Sbci,
    Andi,
    Ori,
    Cpi,
    Ldi,
}

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum OpRdRr
{
    Add,
    Adc,
    Sub,
    Sbc,
    Mul,
    And,
    Or,
    Eor,
    Cpse,
    Cp,
    Cpc,
    Mov,
}

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum OpN
{
    Nop,
}

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum OpK
{
    Jmp,
    Call,
}

impl Instruction
{
    pub fn read<I>(mut bytes: I) -> Result<Self,&'static str>
        where I: Iterator<Item=u8> {

        let b1 = bytes.next().unwrap();
        let b2 = bytes.next().unwrap();

        // must reverse endianess
        let bits16 = ((b2 as u16)<<8) | (b1 as u16);

        if let Some(i) = Self::try_read16(bits16) {
             return Ok(i);
        }

        let b3 = bytes.next().unwrap() as u32;
        let b4 = bytes.next().unwrap() as u32;
        // must reverse endianess
        let bits32 = ((bits16 as u32) << 16) | (b4<<8) | (b3<<0);

        if let Some(i) = Self::try_read32(bits32) {
            return Ok(i);
        }

        Err("unknown instruction")
    }

    pub fn size(self) -> u8 {
        match self {
            Instruction::RdRr(..) => 2,
            Instruction::RdK(..) => 2,
            Instruction::Rd(..) => 2,
            Instruction::N(op) => match op {
                OpN::Nop => 2,
            },
            Instruction::K(op, k) => match op {
                OpK::Jmp => 4,
                OpK::Call => 4,
            },
        }
    }

    fn try_read16(bits: u16) -> Option<Self> {
        if bits == 0 {
            Some(Instruction::N(OpN::Nop))
        } else if let Some(i) = Self::try_read_rd(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_rdk(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_rdrr(bits) {
            Some(i)
        } else {
            None
        }
    }

    pub fn try_read32(bits: u32) -> Option<Self> {
        if let Some(i) = Self::try_read_k(bits) {
            Some(i)
        } else {
            None
        }
    }

    /// rd: `<|opcode|fffd|dddd|ffff|>`.
    fn try_read_rd(bits: u16) -> Option<Self> {
        let opcode = ((bits & 0b1111111000000000) >> 5) |
                      (bits & 0b0000000000001111);

        let rd = ((bits & 0b0000000111110000) >> 4) as u8;

        let op = match opcode {
            0b10010100011 => OpRd::Inc,
            0b10010101010 => OpRd::Dec,
            0b10010100000 => OpRd::Com,
            0b10010100001 => OpRd::Neg,
            0b10010011111 => OpRd::Push,
            0b10010001111 => OpRd::Pop,
            0b10010100010 => OpRd::Swap,
            _ => { return None; },
        };

        Some(Instruction::Rd(op, rd))
    }

    /// rdk: `<|opcode|KKKK|dddd|KKKK|>`
    fn try_read_rdk(bits: u16) -> Option<Self> {
        let opcode = (bits & 0b1111000000000000) >> 12;

        let mut rd =  ((bits & 0b0000000011110000) >> 4) as u8;
        let k =  (((bits & 0b0000111100000000) >> 4) |
                  ((bits & 0b0000000000001111) >> 0)) as u8;

        // RDk registers start from r16 (so range is r16-r31).
        rd += 16;

        let op = match opcode {
            0b0101 => OpRdK::Subi,
            0b0100 => OpRdK::Sbci,
            0b0111 => OpRdK::Andi,
            0b0110 => OpRdK::Ori,
            0b0011 => OpRdK::Cpi,
            0b1110 => OpRdK::Ldi,
            _ => { return None; },
        };

        Some(Instruction::RdK(op, rd, k))
    }

    /// rdrr: `<|opcode|ffrd|dddd|rrrr|>`
    fn try_read_rdrr(bits: u16) -> Option<Self> {
        let opcode = (bits & 0b1111110000000000) >> 10;

        let rd = ((bits & 0b0000000111110000) >> 4) as u8;
        let rr = (((bits & 0b0000001000000000) >> 4) |
                  (bits & 0b0000000000001111)) as u8;

        let op = match opcode {
            0b000011 => OpRdRr::Add,
            0b000111 => OpRdRr::Adc,
            0b000110 => OpRdRr::Sub,
            0b000010 => OpRdRr::Sbc,
            0b100111 => OpRdRr::Mul,
            0b001000 => OpRdRr::And,
            0b001010 => OpRdRr::Or,
            0b001001 => OpRdRr::Eor,
            0b000100 => OpRdRr::Cpse,
            0b000101 => OpRdRr::Cp,
            0b000001 => OpRdRr::Cpc,
            0b001011 => OpRdRr::Mov,
            _ => { return None; },
        };

        Some(Instruction::RdRr(op, rd, rr))
    }

    /// 32-bits branches.
    ///  <|1001|010k|kkkk|fffk|kkkk|kkkk|kkkk|kkkk|>
    fn try_read_k(bits: u32) -> Option<Self> {
        let opcode = (bits & 0xfe000000) >> 25;
        let subopcode = (bits & 0xe0000) >> 17;

        let mut k = ((bits & 0x1f00000) >> 20) |
                     (bits & 0x1ffff);

        // un-left shift the address.
        k <<= 1;

        if opcode != 0b1001010 {
            return None;
        }

        let op = match subopcode {
            0b110 => OpK::Jmp,
            0b111 => OpK::Call,
            _ => { return None; },
        };

        Some(Instruction::K(op, k))
    }
}

// add: 0000 11rd dddd rrrr
