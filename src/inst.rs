use Error;

pub type Gpr = u8;
pub type GprPair = u8;
pub type Address = u32;
pub type RelativeAddress = u32;

#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum Variant
{
    Normal,
    Predecrement,
    Postincrement,
}


/// An instruction.
#[derive(Copy,Clone,Debug,PartialEq,Eq)]
pub enum Instruction
{
    Inc(Gpr),
    Dec(Gpr),
    Com(Gpr),
    Neg(Gpr),
    Push(Gpr),
    Pop(Gpr),
    Swap(Gpr),

    Subi(Gpr, u8),
    Sbci(Gpr, u8),
    Andi(Gpr, u8),
    Ori(Gpr, u8),
    Cpi(Gpr, u8),
    Ldi(Gpr, u8),

    Add(Gpr, Gpr),
    Adc(Gpr, Gpr),
    Sub(Gpr, Gpr),
    Sbc(Gpr, Gpr),
    Mul(Gpr, Gpr),
    And(Gpr, Gpr),
    Or(Gpr, Gpr),
    Eor(Gpr, Gpr),
    Cpse(Gpr, Gpr),
    Cp(Gpr, Gpr),
    Cpc(Gpr, Gpr),
    Mov(Gpr, Gpr),
    Movw(GprPair, GprPair),

    In(Gpr, u8),
    Out(u8, Gpr),
    /// Set bit in IO register.
    Sbi(u8, u8),
    /// Clear bit in IO register.
    Cbi(u8, u8),

    Jmp(u32),
    Call(u32),
    Rjmp(i16),
    Rcall(i16),

    St(GprPair, Gpr, Variant),
    Ld(Gpr, GprPair, Variant),

    Std(GprPair, u8, Gpr),
    Ldd(Gpr, GprPair, u8),

    /// Load program memory.
    /// `GprPair` is always the `Z` register.
    /// The `bool` is whether to postincrement.
    Lpm(Gpr, GprPair, bool),

    Nop,
    Ret,
    Reti,
}

impl Instruction
{
    pub fn read<I>(mut bytes: I) -> Result<Self, Error>
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

        Err(Error::UnknownInstruction)
    }

    pub fn size(self) -> u8 {
        match self {
            Instruction::Jmp(..) => 4,
            Instruction::Call(..) => 4,
            _ => 2,
        }
    }

    fn try_read16(bits: u16) -> Option<Self> {
        if bits == 0 {
            Some(Instruction::Nop)
        } else if bits == 0x9508 {
            Some(Instruction::Ret)
        } else if bits == 0x9518 {
            Some(Instruction::Reti)
        } else if bits == 0x95C8 {
            // LPM (r0, Z implied).
            Some(Instruction::Lpm(0, 30, false))
        } else if let Some(i) = Self::try_read_rd(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_rdk(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_rdrr(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_rda(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_io_ab(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_rdz(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_k16(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_st_ld(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_std_ldd(bits) {
            Some(i)
        } else if let Some(i) = Self::try_read_movw(bits) {
            Some(i)
        } else {
            None
        }
    }

    pub fn try_read32(bits: u32) -> Option<Self> {
        if let Some(i) = Self::try_read_k32(bits) {
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

        match opcode {
            0b10010100011 => Some(Instruction::Inc(rd)),
            0b10010101010 => Some(Instruction::Dec(rd)),
            0b10010100000 => Some(Instruction::Com(rd)),
            0b10010100001 => Some(Instruction::Neg(rd)),
            0b10010011111 => Some(Instruction::Push(rd)),
            0b10010001111 => Some(Instruction::Pop(rd)),
            0b10010100010 => Some(Instruction::Swap(rd)),
            _ => None,
        }
    }

    /// rdk: `<|opcode|KKKK|dddd|KKKK|>`
    fn try_read_rdk(bits: u16) -> Option<Self> {
        let opcode = (bits & 0b1111000000000000) >> 12;

        let mut rd = ((bits & 0b0000000011110000) >> 4) as u8;
        let k     =  (((bits & 0b0000111100000000) >> 4) |
                      ((bits & 0b0000000000001111) >> 0)) as u8;

        // RDk registers start from r16 (so range is r16-r31).
        rd += 16;

        match opcode {
            0b0101 => Some(Instruction::Subi(rd, k)),
            0b0100 => Some(Instruction::Sbci(rd, k)),
            0b0111 => Some(Instruction::Andi(rd, k)),
            0b0110 => Some(Instruction::Ori(rd, k)),
            0b0011 => Some(Instruction::Cpi(rd, k)),
            0b1110 => Some(Instruction::Ldi(rd, k)),
            _ => None,
        }
    }

    /// rdrr: `<|opcode|ffrd|dddd|rrrr|>`
    fn try_read_rdrr(bits: u16) -> Option<Self> {
        let opcode = (bits & 0b1111110000000000) >> 10;

        let rd = ((bits & 0b0000000111110000) >> 4) as u8;
        let rr = (((bits & 0b0000001000000000) >> 4) |
                  (bits & 0b0000000000001111)) as u8;

        match opcode {
            0b000011 => Some(Instruction::Add(rd, rr)),
            0b000111 => Some(Instruction::Adc(rd, rr)),
            0b000110 => Some(Instruction::Sub(rd, rr)),
            0b000010 => Some(Instruction::Sbc(rd, rr)),
            0b100111 => Some(Instruction::Mul(rd, rr)),
            0b001000 => Some(Instruction::And(rd, rr)),
            0b001010 => Some(Instruction::Or(rd, rr)),
            0b001001 => Some(Instruction::Eor(rd, rr)),
            0b000100 => Some(Instruction::Cpse(rd, rr)),
            0b000101 => Some(Instruction::Cp(rd, rr)),
            0b000001 => Some(Instruction::Cpc(rd, rr)),
            0b001011 => Some(Instruction::Mov(rd, rr)),
            _ => None,
        }
    }

    /// Either an `in` or `out` IO instruction.
    /// rda: `1011|fAAd|dddd|AAAA`.
    /// Where `f` is the secondary opcode.
    fn try_read_rda(bits: u16) -> Option<Self> {
        let opcode = (bits & 0xf000) >> 12;
        let subopcode = (bits & 0b100000000000) >> 11;

        let reg = ((0b111110000 & bits) >> 4) as u8;
        let a = (((0b11000000000 & bits) >> 5) |
                 ((0b1111 & bits) >> 0)) as u8;

        if opcode != 0b1011 {
            return None;
        }

        match subopcode {
            0b0 => Some(Instruction::In(reg, a)),
            0b1 => Some(Instruction::Out(a, reg)),
            _ => None,
        }

    }

    /// CBI: 1001 1000 AAAA Abbb
    /// SBI: 1001 1010 AAAA Abbb
    fn try_read_io_ab(bits: u16) -> Option<Self> {
        let opcode = (bits & 0xff00) >> 8;
        let a = ((bits & 0b0000000011111000) >> 3) as u8;
        let b = (bits & 0b111) as u8;

        match opcode {
            0b10011010 => Some(Instruction::Sbi(a, b)),
            0b10011000 => Some(Instruction::Cbi(a, b)),
            _ => None,
        }
    }

    /// `LPM` instructions.
    /// `<1001|000d|dddd|010f>`
    /// `f` is postincrement bit.
    fn try_read_rdz(bits: u16) -> Option<Self> {
        let opcode = (bits & 0b1111111000000000) >> 9;
        let sub_op = bits & 0b1;

        let rd = ((bits & 0x1f0) >> 4) as u8;

        let postinc = sub_op==1;

        match opcode {
            0b1001000 => Some(Instruction::Lpm(rd, 30, postinc)),
            _ => None,
        }
    }

    /// 16-bit relative branches.
    /// `<ffff|kkkk|kkkk|kkkk>`.
    fn try_read_k16(bits: u16) -> Option<Self> {
        let opcode = (bits & 0xf000) >> 12;
        let k = (bits & 0x0fff) as i16;

        match opcode {
            0b1100 => Some(Instruction::Rjmp(k)),
            0b1101 => Some(Instruction::Rcall(k)),
            _ => None,
        }
    }

    /// 32-bit branches.
    /// <|1001|010k|kkkk|fffk|kkkk|kkkk|kkkk|kkkk|>
    fn try_read_k32(bits: u32) -> Option<Self> {
        let opcode = (bits & 0xfe000000) >> 25;
        let subopcode = (bits & 0xe0000) >> 17;

        let mut k = ((bits & 0x1f00000) >> 20) |
                     (bits & 0x1ffff);

        // un-left shift the address.
        k <<= 1;

        if opcode != 0b1001010 {
            return None;
        }

        match subopcode {
            0b110 => Some(Instruction::Jmp(k)),
            0b111 => Some(Instruction::Call(k)),
            _ => None,
        }
    }

    /// Attempts to read an `LD` or `ST` instruction.
    fn try_read_st_ld(bits: u16) -> Option<Self> {
        let opcode = (bits & 0b1111111000000000) >> 9;
        let subop = bits & 0xf;

        let reg = ((bits & 0x1f0) >> 4) as u8;

        match (opcode, subop) {
            (0b1001001, 0b1100) => Some(Instruction::St(26, reg, Variant::Normal)),
            (0b1001001, 0b1101) => Some(Instruction::St(26, reg, Variant::Postincrement)),
            (0b1001001, 0b1110) => Some(Instruction::St(26, reg, Variant::Predecrement)),
            (0b1000001, 0b1000) => Some(Instruction::St(28, reg, Variant::Normal)),
            (0b1001001, 0b1001) => Some(Instruction::St(28, reg, Variant::Postincrement)),
            (0b1001001, 0b1010) => Some(Instruction::St(28, reg, Variant::Predecrement)),
            (0b1000001, 0b0000) => Some(Instruction::St(30, reg, Variant::Normal)),
            (0b1001001, 0b0001) => Some(Instruction::St(30, reg, Variant::Postincrement)),
            (0b1001001, 0b0010) => Some(Instruction::St(30, reg, Variant::Predecrement)),

            (0b1001000, 0b1100) => Some(Instruction::Ld(reg, 26, Variant::Normal)),
            (0b1001000, 0b1101) => Some(Instruction::Ld(reg, 26, Variant::Postincrement)),
            (0b1001000, 0b1110) => Some(Instruction::Ld(reg, 26, Variant::Predecrement)),
            (0b1000000, 0b1000) => Some(Instruction::Ld(reg, 28, Variant::Normal)),
            (0b1001000, 0b1001) => Some(Instruction::Ld(reg, 28, Variant::Postincrement)),
            (0b1001000, 0b1010) => Some(Instruction::Ld(reg, 28, Variant::Predecrement)),
            (0b1000000, 0b0000) => Some(Instruction::Ld(reg, 30, Variant::Normal)),
            (0b1001000, 0b0001) => Some(Instruction::Ld(reg, 30, Variant::Postincrement)),
            (0b1001000, 0b0010) => Some(Instruction::Ld(reg, 30, Variant::Predecrement)),

            _ => None,
        }
    }

    /// An `STD` or `LDD` instruction.
    /// `(std|ldd) rd, y+z => 10q0 qqfr rrrr pqqq`
    /// * `f` is type (`1` for `std`, `0` for `ldd`).
    /// * `p` is PTRREG (`1` for `Y` and `0` for `Z`)
    ///
    fn try_read_std_ldd(bits: u16) -> Option<Self> {
        let opcode = (bits & 0b1101_0000_0000_0000) >> 12;

        let f = (bits & 0b0000_00010_0000_0000) >> 9;
        let p = (bits & 0b1000) >> 3;
        let q = ((bits & 0b0010_0000_0000_0000) >> 7) |
                ((bits & 0b0000_1100_0000_0000) >> 6) |
                ((bits & 0b0000_0000_0000_0111) >> 0);

        let reg = ((bits & 0b1_1111_0000) >> 4) as u8;
        let imm = q as u8;

        if opcode != 0b1000 {
            return None;
        }

        let ptrreg = match p {
            0b0 => 30, // Z reg
            0b1 => 28, // Y reg
            _ => unreachable!(),
        };

        match f {
            0b0 => Some(Instruction::Ldd(reg, ptrreg, imm)),
            0b1 => Some(Instruction::Std(ptrreg, imm, reg)),
            _ => unreachable!(),
        }
    }

    fn try_read_movw(bits: u16) -> Option<Self> {
        let opcode = (bits & 0xff00) >> 8;

        let mut rd = (bits & 0x00f0) >> 4;
        let mut rr = (bits & 0x000f) >> 0;

        // Because all registers are pairs, the last bit is always 0
        // so it is always encoded by right shifting by one.
        rd <<= 1; rr <<= 1;

        if opcode != 0b00000001 {
            return None;
        }

        Some(Instruction::Movw(rd as u8, rr as u8))
    }
}

