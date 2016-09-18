pub mod binary;
pub mod textual;

pub type Gpr = u8;
pub type GprPair = u8;
pub type Address = u32;
pub type RelativeAddress = u32;
pub type RelativeAddress7 = i8;

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

    Brbs(u8, RelativeAddress7),
    Brbc(u8, RelativeAddress7),
    Breq(RelativeAddress7),
    Brne(RelativeAddress7),
    Brcs(RelativeAddress7),
    Brcc(RelativeAddress7),
    Brsh(RelativeAddress7),
    Brlo(RelativeAddress7),
    Brmi(RelativeAddress7),
    Brpl(RelativeAddress7),
    Brge(RelativeAddress7),
    Brlt(RelativeAddress7),
    Brhs(RelativeAddress7),
    Brhc(RelativeAddress7),
    Brts(RelativeAddress7),
    Brtc(RelativeAddress7),
    Brvs(RelativeAddress7),
    Brvc(RelativeAddress7),
    Brie(RelativeAddress7),
    Brid(RelativeAddress7),

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
    pub fn size(self) -> u8 {
        match self {
            Instruction::Jmp(..) => 4,
            Instruction::Call(..) => 4,
            _ => 2,
        }
    }

}

