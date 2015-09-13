
use regs::{self,RegisterFile};
use mem;
use mcu::Mcu;
use inst;


pub const SPACE_REGISTER_OFFSET: u16 = 0;
pub const SPACE_IO_OFFSET: u16 = 0x20;
pub const SPACE_DATA_OFFSET: u16 = 0x60;

/// The AVR CPU.
/// TODO: this should probably be renamed to Mcu (it owns the RAM, etc).
pub struct Cpu
{
    register_file: RegisterFile,

    program_space: mem::Space,
    data_space: mem::Space,

    /// The program counter.
    pc: u32,
}

impl Cpu
{
    pub fn new<M>() -> Self
        where M: Mcu
    {
        Cpu {
            register_file: M::register_file(),
            program_space: mem::Space::new(M::flash_size()),
            data_space: mem::Space::new(M::sram_size()),

            pc: 0,
        }
    }

    pub fn load_program_space<I>(&mut self, bytes: I)
        where I: Iterator<Item=u8> {

        self.program_space.load(bytes);
    }

    pub fn tick(&mut self) {
        let inst = self.fetch();

        println!("Executing {:?}", inst);
        self.execute(inst);
    }

    pub fn register_file(&self) -> &RegisterFile { &self.register_file }
    pub fn program_space(&self) -> &mem::Space { &self.program_space }
    pub fn data_space(&self) -> &mem::Space { &self.data_space }

    /// lhs = lhs + rhs
    pub fn add(&mut self, lhs: u8, rhs: u8) {
        let sum = self.do_rdrr(lhs, rhs, |a,b| a+b);
        self.update_sreg_arithmetic(sum);
    }

    pub fn adc(&mut self, lhs: u8, rhs: u8) {
        let carry = self.register_file.sreg_flag(regs::CARRY_MASK);
        let constant = if carry { 1 } else { 0 };

        let sum = self.do_rdrr(lhs, rhs, |a,b| a+b+constant);
        self.update_sreg_arithmetic(sum);
    }

    /// lhs = lhs - rhs
    pub fn sub(&mut self, lhs: u8, rhs: u8) {
        let diff = self.do_rdrr(lhs, rhs, |a,b| a-b);
        self.update_sreg_arithmetic(diff);
    }

    pub fn sbc(&mut self, lhs: u8, rhs: u8) {
        let carry = self.register_file.sreg_flag(regs::CARRY_MASK);
        let constant = if carry { 1 } else { 0 };

        let diff = self.do_rdrr(lhs, rhs, |a,b| a-b-constant);
        self.update_sreg_arithmetic(diff);
    }

    pub fn subi(&mut self, rd: u8, imm: u8) {
        let diff = self.do_rdi(rd, |d| d-imm as u16);
        self.update_sreg_arithmetic(diff);
    }

    pub fn sbci(&mut self, rd: u8, imm: u8) {
        let diff = self.do_rdi(rd, |d| d-imm as u16);
        self.update_sreg_arithmetic(diff);
    }

    /// R1:R0 = Rd * Rr
    pub fn mul(&mut self, rd: u8, rr: u8) {
        let product = (rd as u16) * (rr as u16);

        let lo = (product & 0x00ff) as u8;
        let hi = ((product & 0xff00) >> 8) as u8;

        *self.register_file.gpr_mut(0).unwrap() = lo;
        *self.register_file.gpr_mut(1).unwrap() = hi;

        self.update_sreg_arithmetic(product);
    }

    pub fn and(&mut self, lhs: u8, rhs: u8) {
        self.do_rdrr(lhs, rhs, |a,b| a&b);
    }

    pub fn andi(&mut self, rd: u8, imm: u8) {
        self.do_rdi(rd, |d| d&imm as u16);
    }

    pub fn or(&mut self, lhs: u8, rhs: u8) {
        self.do_rdrr(lhs, rhs, |a,b| a|b);
    }

    pub fn ori(&mut self, rd: u8, imm: u8) {
        self.do_rdi(rd, |d| d&imm as u16);
    }

    pub fn eor(&mut self, lhs: u8, rhs: u8) {
        self.do_rdrr(lhs, rhs, |a,b| a^b);
    }

    pub fn com(&mut self, rd: u8) {
        self.do_rd(rd, |a| 0xff-a)
    }

    pub fn neg(&mut self, rd: u8) {
        self.do_rd(rd, |a| -(a as i8) as u8)
    }

    pub fn mov(&mut self, lhs: u8, rhs: u8) {
        self.do_rdrr(lhs, rhs, |_,b| b);
    }

    pub fn movw(&mut self, lhs: u8, rhs: u8) {
        self.do_rdrr16(lhs, rhs, |_,b| b)
    }

    pub fn lsl(&mut self, rd: u8) {
        self.do_rd(rd, |d| d<<1)
    }

    pub fn lsr(&mut self, rd: u8) {
        self.do_rd(rd, |d| d>>1)
    }

    pub fn inc(&mut self, rd: u8) {
        self.do_rd(rd, |d| d+1)
    }

    pub fn dec(&mut self, rd: u8) {
        self.do_rd(rd, |d| d-1)
    }

    pub fn push(&mut self, rd: u8) {
        let rd_val = self.register_file.gpr_val(rd).unwrap();
        let sp = self.register_file.gpr_mut(regs::SP_LO_NUM).unwrap();

        assert!(*sp > 0, "stack overflow");

        self.data_space.set_u8(*sp as usize, rd_val);

        *sp -= 1;
    }

    pub fn pop(&mut self, rd: u8) {
        let rd_val = self.register_file.gpr_val(rd).unwrap();

        let sp = self.register_file.gpr_mut(regs::SP_LO_NUM).unwrap();
        *sp += 1;

        assert!(*sp > 0, "stack overflow");

        self.data_space.set_u8(*sp as usize, rd_val);
    }

    pub fn swap(&mut self, rd: u8) {
        self.do_rd(rd, |d| {
            let lo = d & 0x0f;
            let hi = d & 0xf0;

            (lo << 4) | (hi >> 4)
        })
    }

    pub fn cp(&mut self, rd: u8, rr: u8) {
        let rd_val = self.register_file.gpr_val(rd).unwrap() as u16;
        let rr_val = self.register_file.gpr_val(rr).unwrap() as u16;

        self.update_sreg_cp(rd_val, rr_val);
    }

    pub fn cpc(&mut self, rd: u8, rr: u8) {
        unimplemented!()
    }

    pub fn cpse(&mut self, rd: u8, rr: u8) {
        unimplemented!()
    }

    pub fn cpi(&mut self, rd: u8, imm: u8) {
        unimplemented!()
    }

    pub fn ldi(&mut self, rd: u8, imm: u8) {
        self.do_rd(rd, |_| imm);
    }

    pub fn jmp(&mut self, k: u32) {
        self.pc = k;
    }

    pub fn call(&mut self, k: u32) {
        let return_addr = (self.pc + 4) as u16; // after CALL instruction.

        // push return address onto stack
        let mut sp = self.register_file.gpr_pair_val(regs::SP_LO_NUM).unwrap();
        println!("SP: {}", sp);

        self.data_space.set_u16((sp - 1) as usize, return_addr);

        // post-decrement
        sp -= 2;

        self.register_file.set_gpr_pair(regs::SP_LO_NUM, sp);

        self.pc = k;
    }

    pub fn nop(&mut self) { }

    pub fn _in(&mut self, rd: u8, a: u8) {
        // There should only be 6-bits.
        assert!(a <= 0b111111);

        let offset = SPACE_IO_OFFSET + a as u16;
        let io_val = self.data_space.get_u8(offset as usize);

        *self.register_file.gpr_mut(rd).unwrap() = io_val;
    }

    pub fn out(&mut self, a: u8, rd: u8) {
        // There should only be 6-bits.
        assert!(a <= 0b111111);

        let offset = SPACE_IO_OFFSET + a as u16;
        let reg_val = self.register_file.gpr_val(rd).unwrap();

        self.data_space.set_u8(offset as usize, reg_val);
    }

    fn fetch(&mut self) -> inst::Instruction {
        let bytes = self.program_space.bytes()
                                      .skip(self.pc as usize)
                                      .map(|&a| a);

        let inst = inst::Instruction::read(bytes).unwrap();

        self.pc += inst.size() as u32;

        inst
    }

    fn execute(&mut self, inst: inst::Instruction) {
        use inst::Instruction;

        match inst {
            Instruction::Inc(rd) => self.inc(rd),
            Instruction::Dec(rd) => self.dec(rd),
            Instruction::Com(rd) => self.com(rd),
            Instruction::Neg(rd) => self.neg(rd),
            Instruction::Push(rd) => self.push(rd),
            Instruction::Pop(rd) => self.pop(rd),
            Instruction::Swap(rd) => self.swap(rd),
            Instruction::Subi(rd, k) => self.subi(rd, k),
            Instruction::Sbci(rd, k) => self.sbci(rd, k),
            Instruction::Andi(rd, k) => self.andi(rd, k),
            Instruction::Ori(rd, k) => self.ori(rd, k),
            Instruction::Cpi(rd, k) => self.cpi(rd, k),
            Instruction::Ldi(rd, k) => self.ldi(rd, k),
            Instruction::Add(rd, rr) => self.add(rd, rr),
            Instruction::Adc(rd, rr) => self.adc(rd, rr),
            Instruction::Sub(rd, rr) => self.sub(rd, rr),
            Instruction::Sbc(rd, rr) => self.sbc(rd, rr),
            Instruction::Mul(rd, rr) => self.mul(rd, rr),
            Instruction::And(rd, rr) => self.and(rd, rr),
            Instruction::Or(rd, rr) => self.or(rd, rr),
            Instruction::Eor(rd, rr) => self.eor(rd, rr),
            Instruction::Cpse(rd, rr) => self.cpse(rd, rr),
            Instruction::Cp(rd, rr) => self.cp(rd, rr),
            Instruction::Cpc(rd, rr) => self.cpc(rd, rr),
            Instruction::Mov(rd, rr) => self.mov(rd, rr),
            Instruction::Nop => self.nop(),
            Instruction::In(rd, a) => self._in(rd, a),
            Instruction::Out(a, rd) => self.out(a, rd),
            Instruction::Jmp(k) => self.jmp(k),
            Instruction::Call(k) => self.call(k),
        }
    }

    fn do_rd<F>(&mut self, rd: u8, mut f: F)
        where F: FnMut(u8) -> u8 {

        let rd_reg = self.register_file.gpr_mut(rd).unwrap();
        let rd_val = *rd_reg;

        *rd_reg = f(rd_val)
    }

    /// Returns the value of `rd` after execution.
    fn do_rdrr<F>(&mut self, rd: u8, rr: u8, mut f: F) -> u16
        where F: FnMut(u16,u16) -> u16 {

        let rr_val = *self.register_file.gpr(rr).unwrap() as u16;
        let rd_reg = self.register_file.gpr_mut(rd).unwrap();
        let rd_val = (*rd_reg) as u16;

        let val = f(rd_val, rr_val);
        *rd_reg = val as u8;
        val
    }

    fn do_rdi<F>(&mut self, rd: u8, mut f: F) -> u16
        where F: FnMut(u16) -> u16 {
        
        let rd_reg = self.register_file.gpr_mut(rd).unwrap();
        let rd_val = *rd_reg as u16;

        let val = f(rd_val);
        *rd_reg = val as u8;
        val
    }

    fn do_rdrr16<F>(&mut self, rd: u8, rr: u8,  mut f: F)
        where F: FnMut(u16,u16) -> u16 {
        assert!(rd % 2 == 0 && rr % 2 == 0,
                "GPR pairs must be even numbers");

        let rr_val_lo = *self.register_file.gpr(rr).unwrap() as u16;
        let rr_val_hi = *self.register_file.gpr(rr+1).unwrap() as u16;
        let rr_val = (rr_val_hi << 8) | rr_val_lo;

        let rd_val_lo = *self.register_file.gpr(rd).unwrap() as u16;
        let rd_val_hi = *self.register_file.gpr(rd+1).unwrap() as u16;
        let rd_val = (rd_val_hi << 8) | rd_val_lo;

        let val = f(rd_val, rr_val);
        let val_lo = (val & 0x0f) >> 0;
        let val_hi = (val & 0xf0) >> 8;

        *self.register_file.gpr_mut(rd).unwrap() = val_lo as u8;
        *self.register_file.gpr_mut(rd+1).unwrap() = val_hi as u8;
    }

    /// Updates the `V`, `C`, `H`, `N`, `Z`, and `S` status flags.
    fn update_sreg_arithmetic(&mut self, val: u16) {
        self.update_overflow_flag(val);
        self.update_carry_flag(val);
        self.update_half_carry_flag(val);
        self.update_negative_flag(val);
        self.update_zero_flag(val);
    }

    /// Updates the `V`, `C`, `H`, `N`, `Z`, and `S` status flags.
    fn update_sreg_cp(&mut self, rd_val: u16, rr_val: u16) {
        let val = rd_val-rr_val;

        self.update_overflow_flag(val);
        self.update_negative_flag(val);
        self.update_zero_flag(val);

        let is_carry = (rr_val as i16).abs() > (rd_val as i16).abs();
        self.update_status_flag_bit(regs::CARRY_MASK, is_carry);

        // TODO: Set half carry flag
    }

    /// Sets the overflow flag if `val` overflows a `u8`.
    fn update_overflow_flag(&mut self, val: u16) {
        let overflowed = val>0xff;
        self.update_status_flag_bit(regs::OVERFLOW_MASK, overflowed)
    }

    /// Sets the carry flag if necessary.
    fn update_carry_flag(&mut self, val: u16) {
        let is_carry = (val&0b100000000)>0;
        self.update_status_flag_bit(regs::CARRY_MASK, is_carry)
    }

    /// Sets the half carry flag if necessary.
    fn update_half_carry_flag(&mut self, val: u16) {
        let is_hcarry = (val & 0b1000)>0;
        self.update_status_flag_bit(regs::HALF_CARRY_MASK, is_hcarry)
    }

    /// Sets the negative flag based on `val`.
    fn update_negative_flag(&mut self, val: u16) {
        let is_negative = (val & 0b10000000)>0;
        self.update_status_flag_bit(regs::NEGATIVE_MASK, is_negative)
    }

    fn update_zero_flag(&mut self, val: u16) {
        let is_zero = val==0;
        self.update_status_flag_bit(regs::ZERO_MASK, is_zero)
    }

    fn update_status_flag_bit(&mut self, mask: u8, val: bool) {
        let sreg = self.register_file.sreg_mut();

        if val == true {
            *sreg |= mask;
        } else {
            *sreg &= !mask;
        }

        // TODO: update S flag. should be `N xor V`.
    }
}
