use Error;
use regs::{self,RegisterFile};
use mem;
use chips::Chip;
use inst;

/// The address that register space is mapped to in SRAM.
pub const SRAM_REGISTER_OFFSET: u16 = 0;
/// The address that IO space is mapped to in SRAM.
pub const SRAM_IO_OFFSET: u16 = 0x20;
/// The address that data space is mapped to in SRAM.
pub const SRAM_DATA_OFFSET: u16 = 0x60;

pub const PTR_SIZE: u16 = 2;

/// The AVR CPU.
pub struct Core
{
    register_file: RegisterFile,

    program_space: mem::Space,
    memory: mem::Space,

    /// The program counter.
    pc: u32,
}

impl Core
{
    pub fn new<M>() -> Self
        where M: Chip
    {
        Core {
            register_file: M::register_file(),
            program_space: mem::Space::new(M::flash_size()),
            memory: mem::Space::new(M::memory_size()),

            pc: 0,
        }
    }

    pub fn load_program_space<I>(&mut self, bytes: I)
        where I: Iterator<Item=u8> {

        self.program_space.load(bytes);
    }

    pub fn tick(&mut self) -> Result<(), Error> {
        let inst = self.fetch()?;

        println!("Executing {:?}", inst);
        self.execute(inst)
    }

    pub fn register_file(&self) -> &RegisterFile { &self.register_file }
    pub fn register_file_mut(&mut self) -> &mut RegisterFile { &mut self.register_file }

    pub fn program_space(&self) -> &mem::Space { &self.program_space }
    pub fn program_space_mut(&mut self) -> &mut mem::Space { &mut self.program_space }

    pub fn memory(&self) -> &mem::Space { &self.memory }
    pub fn memory_mut(&mut self) -> &mut mem::Space { &mut self.memory }

    /// lhs = lhs + rhs
    pub fn add(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        let sum = self.do_rdrr(lhs, rhs, |a,b| a+b)?;
        self.update_sreg_arithmetic(sum)
    }

    pub fn adc(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        let carry = self.register_file.sreg_flag(regs::CARRY_FLAG);
        let constant = if carry { 1 } else { 0 };

        let sum = self.do_rdrr(lhs, rhs, |a,b| a+b+constant)?;
        self.update_sreg_arithmetic(sum)
    }

    /// lhs = lhs - rhs
    pub fn sub(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        let diff = self.do_rdrr(lhs, rhs, |a,b| a-b)?;
        self.update_sreg_arithmetic(diff)
    }

    pub fn sbc(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        let carry = self.register_file.sreg_flag(regs::CARRY_FLAG);
        let constant = if carry { 1 } else { 0 };

        let diff = self.do_rdrr(lhs, rhs, |a,b| a-b-constant)?;
        self.update_sreg_arithmetic(diff)
    }

    pub fn subi(&mut self, rd: u8, imm: u8) -> Result<(), Error> {
        let diff = self.do_rdi(rd, |d| d-imm as u16)?;
        self.update_sreg_arithmetic(diff)
    }

    pub fn sbci(&mut self, rd: u8, imm: u8) -> Result<(), Error> {
        let diff = self.do_rdi(rd, |d| d-imm as u16)?;
        self.update_sreg_arithmetic(diff)
    }

    /// R1:R0 = Rd * Rr
    pub fn mul(&mut self, rd: u8, rr: u8) -> Result<(), Error> {
        let product = (rd as u16) * (rr as u16);

        let lo = (product & 0x00ff) as u8;
        let hi = ((product & 0xff00) >> 8) as u8;

        *self.register_file.gpr_mut(0).unwrap() = lo;
        *self.register_file.gpr_mut(1).unwrap() = hi;

        self.update_sreg_arithmetic(product)
    }

    pub fn and(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        self.do_rdrr(lhs, rhs, |a,b| a&b)?;
        Ok(())
    }

    pub fn andi(&mut self, rd: u8, imm: u8) -> Result<(), Error> {
        self.do_rdi(rd, |d| d&imm as u16)?;
        Ok(())
    }

    pub fn or(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        self.do_rdrr(lhs, rhs, |a,b| a|b)?;
        Ok(())
    }

    pub fn ori(&mut self, rd: u8, imm: u8) -> Result<(), Error> {
        self.do_rdi(rd, |d| d&imm as u16)?;
        Ok(())
    }

    pub fn eor(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        self.do_rdrr(lhs, rhs, |a,b| a^b)?;
        Ok(())
    }

    pub fn com(&mut self, rd: u8) -> Result<(), Error> {
        self.do_rd(rd, |a| 0xff-a)
    }

    pub fn neg(&mut self, rd: u8) -> Result<(), Error> {
        self.do_rd(rd, |a| -(a as i8) as u8)
    }

    pub fn mov(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        self.do_rdrr(lhs, rhs, |_,b| b)?;
        Ok(())
    }

    pub fn movw(&mut self, lhs: u8, rhs: u8) -> Result<(), Error> {
        self.do_rdrr16(lhs, rhs, |_,b| b)
    }

    pub fn lsl(&mut self, rd: u8) -> Result<(), Error> {
        self.do_rd(rd, |d| d<<1)
    }

    pub fn lsr(&mut self, rd: u8) -> Result<(), Error> {
        self.do_rd(rd, |d| d>>1)
    }

    pub fn inc(&mut self, rd: u8) -> Result<(), Error> {
        self.do_rd(rd, |d| d+1)
    }

    pub fn dec(&mut self, rd: u8) -> Result<(), Error> {
        self.do_rd(rd, |d| d-1)
    }

    pub fn push(&mut self, rd: u8) -> Result<(), Error> {
        let rd_val = self.register_file.gpr_val(rd).unwrap();
        let sp = self.register_file.gpr_mut(regs::SP_LO_NUM).unwrap();

        assert!(*sp > 0, "stack overflow");

        self.memory.set_u8(*sp as usize, rd_val)?;

        *sp -= 1;
        Ok(())
    }

    pub fn pop(&mut self, rd: u8) -> Result<(), Error> {
        let rd_val = self.register_file.gpr_val(rd).unwrap();

        let sp = self.register_file.gpr_mut(regs::SP_LO_NUM).unwrap();
        *sp += 1;

        assert!(*sp > 0, "stack overflow");

        self.memory.set_u8(*sp as usize, rd_val)
    }

    pub fn swap(&mut self, rd: u8) -> Result<(), Error> {
        self.do_rd(rd, |d| {
            let lo = d & 0x0f;
            let hi = d & 0xf0;

            (lo << 4) | (hi >> 4)
        })
    }

    pub fn cp(&mut self, rd: u8, rr: u8) -> Result<(), Error> {
        let rd_val = self.register_file.gpr_val(rd).unwrap() as u16;
        let rr_val = self.register_file.gpr_val(rr).unwrap() as u16;

        self.update_sreg_cp(rd_val, rr_val);
        Ok(())
    }

    pub fn cpc(&mut self, _rd: u8, _rr: u8) -> Result<(), Error> {
        unimplemented!();
    }

    pub fn cpse(&mut self, _rd: u8, _rr: u8) -> Result<(), Error> {
        unimplemented!();
    }

    pub fn cpi(&mut self, _rd: u8, _imm: u8) -> Result<(), Error> {
        Ok(())
    }

    pub fn ldi(&mut self, rd: u8, imm: u8) -> Result<(), Error> {
        self.do_rd(rd, |_| imm)
    }

    pub fn jmp(&mut self, k: u32) -> Result<(), Error> {
        self.pc = k;
        Ok(())
    }

    pub fn call(&mut self, k: u32) -> Result<(), Error> {
        let return_addr = (self.pc + 4) as u16; // after CALL instruction.

        // push return address onto stack
        let mut sp = self.register_file.gpr_pair_val(regs::SP_LO_NUM).unwrap();
        self.memory.set_u16((sp - 1) as usize, return_addr)?;

        // post-decrement
        sp -= 2;

        self.register_file.set_gpr_pair(regs::SP_LO_NUM, sp);

        self.pc = k;
        Ok(())
    }

    pub fn rjmp(&mut self, k: i16) -> Result<(), Error> {
        let pc = self.pc as i32 + k as i32;
        self.pc = pc as u32;
        Ok(())
    }

    pub fn rcall(&mut self, _k: i16) -> Result<(), Error> {
        Ok(())
    }

    pub fn ret(&mut self) -> Result<(), Error>{
        let mut sp = self.register_file.gpr_pair_val(regs::SP_LO_NUM).unwrap();

        // pre-increment
        sp += 2;

        let return_addr = self.memory.get_u16((sp - 1) as usize)?;
        self.register_file.set_gpr_pair(regs::SP_LO_NUM, sp);

        self.pc = return_addr as u32;
        Ok(())
    }

    pub fn reti(&mut self) -> Result<(), Error> {
        self.ret()?;

        self.register_file.sreg_flag_set(regs::INTERRUPT_FLAG);
        Ok(())
    }

    pub fn lpm(&mut self, _rd: u8, _z: u8, _postinc: bool) -> Result<(), Error> {
        unimplemented!();
    }

    pub fn nop(&mut self) -> Result<(), Error> { Ok(()) }

    pub fn _in(&mut self, rd: u8, a: u8) -> Result<(), Error> {
        // There should only be 6-bits.
        assert!(a <= 0b111111);

        let offset = SRAM_IO_OFFSET + a as u16;
        let io_val = self.memory.get_u8(offset as usize)?;

        *self.register_file.gpr_mut(rd).unwrap() = io_val;
        Ok(())
    }

    pub fn out(&mut self, a: u8, rd: u8) -> Result<(), Error> {
        // There should only be 6-bits.
        assert!(a <= 0b111111);

        let offset = SRAM_IO_OFFSET + a as u16;
        let reg_val = self.register_file.gpr_val(rd).unwrap();

        self.memory.set_u8(offset as usize, reg_val)
    }

    pub fn sbi(&mut self, a: u8, b: u8) -> Result<(), Error> {
        unimplemented!();
    }

    pub fn cbi(&mut self, a: u8, b: u8) -> Result<(), Error> {
        unimplemented!();
    }

    fn st(&mut self, ptr: u8, reg: u8, variant: inst::Variant) -> Result<(), Error> {
        let addr = self.register_file.gpr_pair_val(ptr).unwrap();
        let val = self.register_file.gpr_val(reg).unwrap();

        self.memory.set_u8(addr as usize, val)?;

        self.handle_ld_st_variant(ptr, variant);
        Ok(())
    }

    fn ld(&mut self, reg: u8, ptr: u8, variant: inst::Variant) -> Result<(), Error> {
        let addr = self.register_file.gpr_pair_val(ptr).unwrap();

        // Load from data space
        let val = self.memory.get_u8(addr as usize)?;
        // Store to register.
        *self.register_file.gpr_mut(reg).unwrap() = val;

        self.handle_ld_st_variant(ptr, variant);
        Ok(())
    }

    fn std(&mut self, ptr: u8, imm: u8, reg: u8) -> Result<(), Error> {
        let addr = self.register_file.gpr_pair_val(ptr).unwrap() + imm as u16;
        let val = self.register_file.gpr_val(reg).unwrap();

        self.memory.set_u8(addr as usize, val)
    }

    fn ldd(&mut self, reg: u8, ptr: u8, imm: u8) -> Result<(), Error> {
        let addr = self.register_file.gpr_pair_val(ptr).unwrap() + imm as u16;

        let val = self.memory.get_u8(addr as usize)?;

        *self.register_file.gpr_mut(reg).unwrap() = val;
        Ok(())
    }

    fn fetch(&mut self) -> Result<inst::Instruction, Error> {
        let bytes = self.program_space.bytes()
                                      .skip(self.pc as usize)
                                      .map(|&a| a);

        Ok(inst::Instruction::read(bytes).unwrap())
    }

    fn execute(&mut self, inst: inst::Instruction) -> Result<(), Error> {
        use inst::Instruction;

        self.pc += inst.size() as u32;

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
            Instruction::Movw(rd, rr) => self.movw(rd, rr),
            Instruction::Nop => self.nop(),
            Instruction::Ret => self.ret(),
            Instruction::Reti => self.reti(),
            Instruction::In(rd, a) => self._in(rd, a),
            Instruction::Out(a, rd) => self.out(a, rd),
            Instruction::Sbi(a, b) => self.sbi(a, b),
            Instruction::Cbi(a, b) => self.cbi(a, b),
            Instruction::Jmp(k) => self.jmp(k),
            Instruction::Call(k) => self.call(k),
            Instruction::Rjmp(k) => self.rjmp(k),
            Instruction::Rcall(k) => self.rcall(k),
            Instruction::Lpm(rd, z, postinc) => self.lpm(rd, z, postinc),
            Instruction::St(ptr, reg, variant) => self.st(ptr, reg, variant),
            Instruction::Std(ptr, imm, reg) => self.std(ptr, imm, reg),
            Instruction::Ld(reg, ptr, variant) => self.ld(reg, ptr, variant),
            Instruction::Ldd(reg, ptr, imm) => self.ldd(reg, ptr, imm),
        }
    }

    fn do_rd<F>(&mut self, rd: u8, mut f: F) -> Result<(), Error>
        where F: FnMut(u8) -> u8 {

        let rd_reg = self.register_file.gpr_mut(rd).unwrap();
        let rd_val = *rd_reg;

        *rd_reg = f(rd_val);
        Ok(())
    }

    /// Returns the value of `rd` after execution.
    fn do_rdrr<F>(&mut self, rd: u8, rr: u8, mut f: F) -> Result<u16, Error>
        where F: FnMut(u16,u16) -> u16 {

        let rr_val = self.register_file.gpr(rr).unwrap() as u16;
        let rd_reg = self.register_file.gpr_mut(rd).unwrap();
        let rd_val = (*rd_reg) as u16;

        let val = f(rd_val, rr_val);
        *rd_reg = val as u8;
        Ok(val)
    }

    fn do_rdi<F>(&mut self, rd: u8, mut f: F) -> Result<u16, Error>
        where F: FnMut(u16) -> u16 {

        let rd_reg = self.register_file.gpr_mut(rd).unwrap();
        let rd_val = *rd_reg as u16;

        let val = f(rd_val);
        *rd_reg = val as u8;
        Ok(val)
    }

    fn do_rdrr16<F>(&mut self, rd: u8, rr: u8,  mut f: F) -> Result<(), Error>
        where F: FnMut(u16,u16) -> u16 {
        assert!(rd % 2 == 0 && rr % 2 == 0,
                "GPR pairs must be even numbers");

        let rr_val_lo = self.register_file.gpr(rr).unwrap() as u16;
        let rr_val_hi = self.register_file.gpr(rr+1).unwrap() as u16;
        let rr_val = (rr_val_hi << 8) | rr_val_lo;

        let rd_val_lo = self.register_file.gpr(rd).unwrap() as u16;
        let rd_val_hi = self.register_file.gpr(rd+1).unwrap() as u16;
        let rd_val = (rd_val_hi << 8) | rd_val_lo;

        let val = f(rd_val, rr_val);
        let val_lo = (val & 0x0f) >> 0;
        let val_hi = (val & 0xf0) >> 8;

        *self.register_file.gpr_mut(rd).unwrap() = val_lo as u8;
        *self.register_file.gpr_mut(rd+1).unwrap() = val_hi as u8;
        Ok(())
    }

    /// Updates the `V`, `C`, `H`, `N`, `Z`, and `S` status flags.
    fn update_sreg_arithmetic(&mut self, val: u16) -> Result<(), Error> {
        self.update_overflow_flag(val);
        self.update_carry_flag(val);
        self.update_half_carry_flag(val);
        self.update_negative_flag(val);
        self.update_zero_flag(val);
        Ok(())
    }

    /// Updates the `V`, `C`, `H`, `N`, `Z`, and `S` status flags.
    fn update_sreg_cp(&mut self, rd_val: u16, rr_val: u16) {
        let val = rd_val-rr_val;

        self.update_overflow_flag(val);
        self.update_negative_flag(val);
        self.update_zero_flag(val);

        let is_carry = (rr_val as i16).abs() > (rd_val as i16).abs();
        self.update_status_flag_bit(regs::CARRY_FLAG, is_carry);

        // TODO: Set half carry flag
    }

    /// Sets the overflow flag if `val` overflows a `u8`.
    fn update_overflow_flag(&mut self, val: u16) {
        let overflowed = val>0xff;
        self.update_status_flag_bit(regs::OVERFLOW_FLAG, overflowed)
    }

    /// Sets the carry flag if necessary.
    fn update_carry_flag(&mut self, val: u16) {
        let is_carry = (val&0b100000000)>0;
        self.update_status_flag_bit(regs::CARRY_FLAG, is_carry)
    }

    /// Sets the half carry flag if necessary.
    fn update_half_carry_flag(&mut self, val: u16) {
        let is_hcarry = (val & 0b1000)>0;
        self.update_status_flag_bit(regs::HALF_CARRY_FLAG, is_hcarry)
    }

    /// Sets the negative flag based on `val`.
    fn update_negative_flag(&mut self, val: u16) {
        let is_negative = (val & 0b10000000)>0;
        self.update_status_flag_bit(regs::NEGATIVE_FLAG, is_negative)
    }

    fn update_zero_flag(&mut self, val: u16) {
        let is_zero = val==0;
        self.update_status_flag_bit(regs::ZERO_FLAG, is_zero)
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

    fn handle_ld_st_variant(&mut self, ptr: u8, variant: inst::Variant) {
        let mut val = self.register_file.gpr_pair_val(ptr).unwrap();

        match variant {
            inst::Variant::Normal => (),
            inst::Variant::Predecrement => val -= PTR_SIZE,
            inst::Variant::Postincrement => val += PTR_SIZE,
        }

        self.register_file.set_gpr_pair(ptr, val);
    }
}
