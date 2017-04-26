use Instruction;
use nom::{digit, space};
use std::str;
use std::str::FromStr;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct GPR8(pub u8);

named!(gpr8<&[u8], GPR8>,
    chain!(
        tag!("r") ~

        number: map_res!(
            map_res!(
                digit,
                str::from_utf8
            ),
            FromStr::from_str
        ) ,

        ||{ GPR8(number) }
    )
);

named!(gpr2<&[u8], (GPR8, GPR8)>,
    chain!(
        opt!( space ) ~
        rd: gpr8 ~
        opt!( space ) ~
        tag!(",") ~
        opt!( space ) ~
        rr: gpr8 ,

        ||{ (rd, rr) }
    )
);

pub fn parse_instruction(line: &str) -> Result<Instruction, String> {
    let mut words = line.split_whitespace();
    let mnemonic = words.next().unwrap();

    let operands: Vec<_> = words.collect();
    let operands = operands.join(" ");

    macro_rules! handle_gpr2 {
        ($instruction:ident) => {
            {
                let (_, (rd, rr)) = gpr2(operands.as_bytes()).unwrap();
                Ok(Instruction::$instruction(rd.0, rr.0))
            }
        }
    }

    match mnemonic {
        "add" => handle_gpr2!(Add),
        "adc" => handle_gpr2!(Adc),
        "sub" => handle_gpr2!(Sub),
        "sbc" => handle_gpr2!(Sbc),
        "mul" => handle_gpr2!(Mul),
        "and" => handle_gpr2!(And),
        "or" => handle_gpr2!(Or),
        "eor" => handle_gpr2!(Eor),
        "cpse" => handle_gpr2!(Cpse),
        "cp" => handle_gpr2!(Cp),
        "cpc" => handle_gpr2!(Cpc),
        "mov" => handle_gpr2!(Mov),
        "movw" => handle_gpr2!(Movw),
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod test
{
    pub use Instruction;
    pub use super::*;
    pub use nom::{IResult,digit};

    describe! gpr8 {
        it "can parse r0" { assert_eq!(super::super::gpr8(b"r0"), IResult::Done(&b""[..], GPR8(0))); }
        it "can parse r1" { assert_eq!(super::super::gpr8(b"r1"), IResult::Done(&b""[..], GPR8(1))); }
        it "can parse r31" { assert_eq!(super::super::gpr8(b"r31"), IResult::Done(&b""[..], GPR8(31))); }
    }

    describe! gpr2 {
        it "can parse basic registers" {
            assert_eq!(super::super::gpr2(b"r1, r2"), IResult::Done(&b""[..], (GPR8(1), GPR8(2))));
        }
    }

    describe! add {
        it "can parse add" {
            assert_eq!(super::super::parse_instruction("add r1, r2"), Ok(Instruction::Add(1, 2)));
        }
    }
}

