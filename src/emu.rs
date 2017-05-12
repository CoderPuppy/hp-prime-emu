extern crate byteorder;

use instr::*;
use mem;
use std;
use binutil::{mask, bit};
use std::fmt::Debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionSet {
	ARM,
	Thumb,
	Jazelle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
	User,
	FIQ,
	IRQ,
	Supervisor,
	Abort,
	Undefined,
	System,
}

#[derive(Debug, Clone)]
pub struct StatusRegister {
	pub negative: bool, // N
	pub zero: bool, // Z
	pub carry: bool, // C
	pub overflow: bool, // V
	pub dsp_overflow: bool, // Q
	pub instruction_set: InstructionSet, // J, T
	pub disable_irq: bool, // I
	pub disable_fiq: bool, // F
	pub mode: Mode,
}

#[derive(Debug)]
pub struct Registers {
	pub cpsr: StatusRegister,
	pub main: [u32; 16],
	// (lower register (excluding pc), spsr)
	pub fiq: ([u32; 7], StatusRegister),
	pub irq: ([u32; 2], StatusRegister),
	pub svc: ([u32; 2], StatusRegister),
	pub abt: ([u32; 2], StatusRegister),
	pub und: ([u32; 2], StatusRegister),
}
impl Registers {
	pub fn get(&self, reg: Register) -> u32 {
		let mode = self.cpsr.mode;
		if reg.0 <= 7 || (reg.0 <= 12 && mode != Mode::FIQ) || mode == Mode::User || mode == Mode::System || reg.0 == 15 {
			self.main[reg.0 as usize]
		} else {
			match self.cpsr.mode {
				Mode::FIQ => self.fiq.0[reg.0 as usize - 8],
				Mode::IRQ => self.irq.0[reg.0 as usize - 13],
				Mode::Supervisor => self.svc.0[reg.0 as usize - 13],
				Mode::Abort => self.abt.0[reg.0 as usize - 13],
				Mode::Undefined => self.und.0[reg.0 as usize - 13],
				_ => unreachable!(),
			}
		}
	}

	pub fn set(&mut self, reg: Register, val: u32) {
		let mode = self.cpsr.mode;
		if reg.0 <= 7 || (reg.0 <= 12 && mode != Mode::FIQ) || mode == Mode::User || mode == Mode::System || reg.0 == 15 {
			self.main[reg.0 as usize] = val
		} else {
			match self.cpsr.mode {
				Mode::FIQ => self.fiq.0[reg.0 as usize - 8] = val,
				Mode::IRQ => self.irq.0[reg.0 as usize - 13] = val,
				Mode::Supervisor => self.svc.0[reg.0 as usize - 13] = val,
				Mode::Abort => self.abt.0[reg.0 as usize - 13] = val,
				Mode::Undefined => self.und.0[reg.0 as usize - 13] = val,
				_ => unreachable!(),
			}
		}
	}

	pub fn get_spsr(&self) -> Option<&StatusRegister> {
		match self.cpsr.mode {
			Mode::User => None,
			Mode::FIQ => Some(&self.fiq.1),
			Mode::IRQ => Some(&self.irq.1),
			Mode::Supervisor => Some(&self.svc.1),
			Mode::Abort => Some(&self.abt.1),
			Mode::Undefined => Some(&self.und.1),
			Mode::System => None,
		}
	}
}

const DRAM_ADDR: u32 = 0x3000_0000;
const DRAM_SIZE: u32 = 0x0200_0000; // 32 MiB
const SRAM_ADDR: u32 = 0x0000_0000;
const SRAM_SIZE: u32 = 0x0001_0000; // 64 KiB

#[derive(Debug)]
pub enum Error {
	Memory(u32, u32),
}

type Result<a> = std::result::Result<a, Error>;

pub struct Emulator {
	pub dram: mem::OffsetStore<mem::MmapStore>,
	pub sram: mem::OffsetStore<mem::MmapStore>,
	pub nand: nand::Nand,
	pub gpio: mem::OffsetStore<NullStore>,
	pub uart: mem::OffsetStore<NullStore>,
	pub regs: Registers,
}
impl Emulator {
	pub fn new() -> Self {
		let psr = StatusRegister {
			negative: false,
			zero: false,
			carry: false,
			overflow: false,
			dsp_overflow: false,
			instruction_set: InstructionSet::ARM,
			disable_irq: true,
			disable_fiq: true,
			mode: Mode::Supervisor,
		};
		Emulator {
			dram: mem::OffsetStore(DRAM_ADDR, mem::MmapStore::new(DRAM_SIZE)),
			sram: mem::OffsetStore(SRAM_ADDR, mem::MmapStore::new(SRAM_SIZE)),
			nand: nand::Nand::new(),
			gpio: mem::OffsetStore(gpio::BASE_ADDR, NullStore::new(gpio::SIZE)),
			uart: mem::OffsetStore(uart::BASE_ADDR, NullStore::new(uart::SIZE)),
			regs: Registers {
				cpsr: psr.clone(),
				main: [0; 16],
				fiq: ([0; 7], psr.clone()),
				irq: ([0; 2], psr.clone()),
				svc: ([0; 2], psr.clone()),
				abt: ([0; 2], psr.clone()),
				und: ([0; 2], psr.clone()),
			},
		}
	}

	pub fn memories<'a>(&'a self) -> Vec<&'a mem::Store> { vec![
		&self.sram,
		&self.dram,
		&self.nand,
		&self.uart,
		&self.gpio
	] }
	pub fn memories_mut<'a>(&'a mut self) -> Vec<&'a mut mem::Store> { vec![
		&mut self.sram,
		&mut self.dram,
		&mut self.nand,
		&mut self.uart,
		&mut self.gpio
	] }
	pub fn find_store(&self, begin: u32, end: u32) -> Result<&mem::Store> {
		for store in self.memories().iter() {
			if store.min_addr() > begin {
				break
			} else if end <= store.max_addr() {
				return Ok(*store)
			}
		}
		Err(Error::Memory(begin, end))
	}
	pub fn find_store_mut<'a>(memories: &'a mut [&'a mut mem::Store], begin: u32, end: u32) -> Result<&'a mut mem::Store> {
		for store in memories {
			if store.min_addr() > begin {
				break
			} else if end <= store.max_addr() {
				return Ok(*store)
			}
		}
		Err(Error::Memory(begin, end))
	}

	pub fn load_byte(&self, addr: u32) -> Result<u8> {
		self.find_store(addr, addr)?.load_byte(addr).ok_or(Error::Memory(addr, addr))
	}
	pub fn load_halfword(&self, addr: u32) -> Result<u16> {
		self.find_store(addr, addr + 1)?.load_halfword(addr).ok_or(Error::Memory(addr, addr + 1))
	}
	pub fn load_word(&self, addr: u32) -> Result<u32> {
		self.find_store(addr, addr + 3)?.load_word(addr).ok_or(Error::Memory(addr, addr + 3))
	}

	pub fn store_byte(&mut self, addr: u32, data: u8) -> Result<()> {
		if Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr)?.store_byte(addr, data) {
			Ok(())
		} else {
			Err(Error::Memory(addr, addr))
		}
	}
	pub fn store_halfword(&mut self, addr: u32, data: u16) -> Result<()> {
		if Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr + 1)?.store_halfword(addr, data) {
			Ok(())
		} else {
			Err(Error::Memory(addr, addr + 1))
		}
	}
	pub fn store_word(&mut self, addr: u32, data: u32) -> Result<()> {
		if Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr + 3)?.store_word(addr, data) {
			Ok(())
		} else {
			Err(Error::Memory(addr, addr + 3))
		}
	}

	pub fn get_reg_op(&self, reg: Register) -> u32 {
		if reg == Register(15) {
			self.regs.get(Register(15)) + 4
		} else {
			self.regs.get(reg)
		}
	}
	pub fn get_shift_op_shift(&self, shift: ShiftOpShift) -> u32 {
		match shift {
			ShiftOpShift::Immediate(v) => v as u32,
			ShiftOpShift::Register(reg) => self.get_reg_op(reg),
		}
	}
	pub fn get_shift_op(&self, base: u32, op: ShiftOp) -> (bool, u32) {
		match op {
			ShiftOp::LSL(shift) => {
				let shift = self.get_shift_op_shift(shift) & mask(8);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else if shift <= 32 {
					base >> 32 - shift & 0b1 == 0b1
				} else {
					false
				}, base << shift)
			},
			ShiftOp::LSR(ShiftOpShift::Immediate(0)) => (base >> 31 & 0b1 == 0b1, 0), // unsigned base >> 32
			ShiftOp::LSR(shift) => {
				let shift = self.get_shift_op_shift(shift) & mask(8);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else if shift <= 32 {
					base >> shift - 1 & 0b1 == 0b1
				} else {
					false
				}, base >> shift)
			},
			ShiftOp::ASR(ShiftOpShift::Immediate(0)) => if base >> 31 & 0b1 == 0b1 { // signed base >> 32
				(true, u32::max_value())
			} else {
				(false, 0)
			},
			ShiftOp::ASR(shift) => {
				let shift = self.get_shift_op_shift(shift) & mask(8);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else if shift <= 32 {
					base >> shift - 1 & 0b1 == 0b1
				} else {
					base >> 31 & 0b1 == 0b1
				}, (base as i32 >> shift) as u32)
			},
			ShiftOp::ROR(shift) => {
				let shift = self.get_shift_op_shift(shift) & mask(8);
				let res = base.rotate_right(shift);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else {
					res >> 31 & 0b1 == 0b1
				}, res)
			},
			ShiftOp::RRX => {
				(base & 0b1 == 0b1, base >> 1 | bit(self.regs.cpsr.carry) << 31)
			},
		}
	}
	pub fn get_shift(&self, op: ShiftOperand) -> (bool, u32) {
		match op {
			ShiftOperand::Immediate { imm, rotate } => {
				let val = (imm as u32).rotate_right(rotate as u32 * 2);
				(if rotate == 0 { self.regs.cpsr.carry } else { val >> 31 & 0b1 == 0b1 }, val)
			},
			ShiftOperand::Register { reg, shift } => {
				let base = self.get_reg_op(reg);
				self.get_shift_op(base, shift)
			},
		}
	}

	pub fn get_addr_mode(&self, mode: AddrMode) -> u32 {
		match mode {
			AddrMode::Register { reg, shift } => {
				let base = self.get_reg_op(reg);
				self.get_shift_op(base, shift).1
			},
			AddrMode::Immediate(i) => i as u32,
		}
	}

	pub fn put_mul_dest<N: Debug, A: Debug, SA: Debug, SS: Debug, LN: Debug, LA: Debug>(&mut self, dest: MulDest<N, A, SA, SS, LN, LA>, val: u64) {
		match dest {
			MulDest::SeparateAccumulate(_, rd, ra) => {
				let a = self.get_reg_op(ra);
				self.regs.set(rd, (val + a as u64) as u32);
			},
			_ => panic!("unhandled mul dest: {:?}", dest),
		}
	}

	pub fn check_cond(&self, cond: Condition) -> bool {
		match cond {
			Condition::Equal => self.regs.cpsr.zero,
			Condition::NotEqual => !self.regs.cpsr.zero,
			Condition::UnsignedGTE => self.regs.cpsr.carry,
			Condition::UnsignedLT => !self.regs.cpsr.carry,
			Condition::Negative => self.regs.cpsr.negative,
			Condition::NonNegative => !self.regs.cpsr.negative,
			Condition::Overflow => self.regs.cpsr.overflow,
			Condition::NoOverflow => !self.regs.cpsr.overflow,
			Condition::UnsignedGT => self.regs.cpsr.carry && !self.regs.cpsr.overflow,
			Condition::UnsignedLTE => !self.regs.cpsr.carry || self.regs.cpsr.overflow,
			Condition::SignedGTE => self.regs.cpsr.negative == self.regs.cpsr.overflow,
			Condition::SignedLT => self.regs.cpsr.negative != self.regs.cpsr.overflow,
			Condition::SignedGT => !self.regs.cpsr.zero && self.regs.cpsr.negative == self.regs.cpsr.overflow,
			Condition::SignedLTE => self.regs.cpsr.zero || self.regs.cpsr.negative != self.regs.cpsr.overflow,
			Condition::Always => true,
			Condition::Custom => true,
		}
	}

	pub fn tick(&mut self) {
		assert!(self.regs.main[15] & 0b11 == 0, "PC must be word aligned");
		assert!(self.regs.cpsr.instruction_set == InstructionSet::ARM, "Thumb and Jazelle aren't supported");
		let instr_raw = self.load_word(self.regs.main[15]).unwrap();
		let instr = Instr::disasm(instr_raw);
		println!("{:x} {:?}", self.regs.main[15], instr);
		self.regs.main[15] += 4;
		if self.check_cond(instr.cond()) {
			match instr {
				Instr::Data { cond: _, op, update_status, dest, op1, op2 } => {
					let carry;
					let overflow;
					let out;
					let dest_used;
					match op {
						DataOp::Move => {
							let (carry_, out_) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = out_;
							self.regs.set(dest, out);
							dest_used = true;
						},
						DataOp::Negate => {
							let (carry_, out_) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = !out_;
							self.regs.set(dest, out);
							dest_used = true;
						},
						DataOp::Compare => {
							let a1 = self.get_reg_op(op1);
							let (_, a2) = self.get_shift(op2);
							let (out_, carry_) = a1.overflowing_sub(a2);
							carry = carry_;
							out = out_;
							overflow = Some((a1 >> 31 ^ a2 >> 31) & (a1 >> 31 ^ out >> 31) == 0b1);
							dest_used = false;
						},
						DataOp::Test => {
							let a1 = self.get_reg_op(op1);
							let (carry_, a2) = self.get_shift(op2);
							out = a1 & a2;
							carry = carry_;
							overflow = None;
							dest_used = false;
						},
						DataOp::Or => {
							let (carry_, a2) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = self.get_reg_op(op1) | a2;
							dest_used = true;
						},
						DataOp::XOr => {
							let (carry_, a2) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = self.get_reg_op(op1) ^ a2;
							dest_used = true;
						},
						DataOp::And => {
							let (carry_, a2) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = self.get_reg_op(op1) & a2;
							dest_used = true;
						},
						DataOp::BitClear => {
							let (carry_, a2) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = self.get_reg_op(op1) & !a2;
							dest_used = true;
						},
						DataOp::Add { carry: use_carry } => {
							let a1 = self.get_reg_op(op1);
							let (_, a2) = self.get_shift(op2);
							let (out_, carry_) = a1.overflowing_add(a2);
							if use_carry && self.regs.cpsr.carry {
								let (out__, carry__) = out_.overflowing_add(1);
								panic!("out_: {}, carry_: {}, out__: {}, carry__: {}", out_, carry_, out__, carry__);
							} else {
								carry = carry_;
								out = out_;
							}
							overflow = Some(a1 >> 31 == a2 >> 31 && a1 >> 31 != out >> 31);
							dest_used = true;
							self.regs.set(dest, out);
						},
						DataOp::Subtract { reverse, carry: use_carry } => {
							let (ap, an) = if reverse {
								(self.get_shift(op2).1, self.get_reg_op(op1))
							} else {
								(self.get_reg_op(op1), self.get_shift(op2).1)
							};
							let (out_, carry_) = ap.overflowing_sub(an);
							if use_carry && !self.regs.cpsr.carry {
								let (out__, carry__) = out_.overflowing_sub(1);
								panic!("out_: {}, carry_: {}, out__: {}, carry__: {}", out_, carry_, out__, carry__);
							} else {
								carry = carry_;
								out = out_;
							}
							overflow = Some(ap >> 31 != an >> 31 && ap >> 31 != out >> 31);
							dest_used = true;
							self.regs.set(dest, out);
						},
						op => panic!("unhandled data op: {:?}", op),
					};
					if update_status {
						if dest_used && dest == Register(15) {
							self.regs.cpsr = self.regs.get_spsr().unwrap().clone();
						} else {
							self.regs.cpsr.carry = carry;
							if let Some(overflow) = overflow {
								self.regs.cpsr.overflow = overflow;
							};
							self.regs.cpsr.negative = out >> 31 & 0b1 == 0b1;
							self.regs.cpsr.zero = out == 0;
						}
					}
				},
				Instr::LoadStore { cond: _, load, data: rd, addr: ra, mode, byte, priv_walk, upwards } => {
					let base = self.get_reg_op(ra);
					let offset = self.get_addr_mode(mode);
					let new_addr = if upwards {
						base + offset
					} else {
						base - offset
					};
					let addr = match priv_walk {
						PrivWalk::Post { unprivileged } => {
							if unprivileged {
								panic!("TODO: unprivileged load/store");
							}
							base
						},
						PrivWalk::Pre { walk } => {
							if walk {
								self.regs.set(ra, new_addr);
							}
							new_addr
						},
					};
					if load {
						let val = if byte {
							self.load_byte(addr).unwrap() as u32
						} else {
							self.load_word(addr).unwrap()
						};
						self.regs.set(rd, val)
					} else {
						let data = self.get_reg_op(rd);
						if byte {
							self.store_byte(addr, data as u8).unwrap();
						} else {
							self.store_word(addr, data).unwrap();
						}
					}
					if let PrivWalk::Post { .. } = priv_walk {
						self.regs.set(ra, new_addr);
					}
				},
				Instr::Jump { cond: _, link, addr } => {
					if link {
						let val = self.regs.get(Register(15));
						self.regs.set(Register(14), val)
					}
					let val = self.regs.get(Register(15)).overflowing_add(4).0.overflowing_add((addr as u32) << 2).0;
					self.regs.set(Register(15), val);
				},
				Instr::LoadStatus { cond: _, dst, src } => {
					self.regs.set(dst, panic!("TODO"));
				},
				Instr::LoadStoreMultiple { cond: _, load, base_excluded, upwards, exc_thing, walk, registers, base } => {
					let base_addr = self.get_reg_op(base);
					let offset = if upwards { 4 } else { -4i32 as u32 };
					let num = registers.count_ones();
					let start_addr = base_addr.wrapping_add(if base_excluded {
						offset
					} else {
						0
					});
					let min_addr = if upwards {
						start_addr
					} else {
						start_addr.wrapping_add((offset as i32 * (num as i32 - 1)) as u32)
					};
					let unbanked = exc_thing && registers >> 15 & 0b1 == 0b0;
					let mut addr = min_addr;
					for i in 0 ... 15u8 {
						if registers >> i & 0b1 == 0b1 {
							if load {
								let val = self.load_word(addr).unwrap();
								if unbanked {
									self.regs.main[i as usize] = val
								} else {
									self.regs.set(Register(i), val)
								}
							} else {
								let val = if unbanked {
									self.regs.main[i as usize]
								} else {
									self.get_reg_op(Register(i))
								};
								self.store_word(addr, val).unwrap();
							}
							addr += 4;
						}
					}
					if walk {
						self.regs.set(base, base_addr.wrapping_add((num as i32 * offset as i32) as u32));
					}
					if exc_thing && registers >> 15 & 0b1 == 0b1 {
						self.regs.cpsr = self.regs.get_spsr().unwrap().clone();
					};
				},
				Instr::Thumby { cond: _, link, addr: ra } => {
					let addr = self.get_reg_op(ra);
					if addr & 0b1 == 0b1 {
						panic!("A thumby instruction is trying to go to thumb!!");
					}
					if link {
						let val = self.regs.get(Register(15));
						self.regs.set(Register(14), val)
					}
					self.regs.set(Register(15), addr & !(1 << 0));
				},
				Instr::Mul { cond: _, op, op1, op2 } => {
					let op1 = self.get_reg_op(op1);
					let op2 = self.get_reg_op(op2);
					match op {
						MulOp::Halfword { dest, op1_hi, op2_hi } => {
							let op1 = if op1_hi {
								op1 >> 16
							} else {
								op1
							} & mask(16);
							let op2 = if op2_hi {
								op2 >> 16
							} else {
								op2
							} & mask(16);
							self.put_mul_dest(dest, op1 as u64 * op2 as u64);
						},
						_ => panic!("unhandled mul op: {:?}", op),
					}
				},
				Instr::MiscLoadStore { cond: _, load, data: rd, addr: ra, mode, upwards, walk } => {
					let addr = self.get_reg_op(ra);
					let offset = match mode {
						MiscAddrMode::Immediate(o) => o as u32,
						MiscAddrMode::Register(ro) => self.get_reg_op(ro),
					};
					let offset = if upwards {
						offset
					} else {
						-(offset as i32) as u32
					};
					let new_addr = addr.wrapping_add(offset);
					let addr = match walk {
						MiscWalk::Post => addr,
						MiscWalk::Pre { walk } => {
							if walk {
								self.regs.set(ra, new_addr);
							}
							new_addr
						},
					};
					let val = self.get_reg_op(rd);
					match load {
						MiscLoadStore::Halfword(None) => {
							self.store_halfword(addr, val as u16).unwrap();
						},
						MiscLoadStore::Halfword(Some(signed)) => {
							let val = self.load_halfword(addr).unwrap();
							let out_val = if signed {
								(val as i32).wrapping_shl(16).wrapping_shr(16) as u32
							} else {
								val as u32
							};
							self.regs.set(rd, out_val);
						},
						MiscLoadStore::LoadSignedByte => {
							let out_val = (self.load_byte(addr).unwrap() as i32).wrapping_shl(24).wrapping_shr(24) as u32;
							self.regs.set(rd, out_val)
						},
						MiscLoadStore::Doubleword { load: true } => {
							assert!(rd.0 % 2 == 0);
							let val = self.load_word(addr).unwrap();
							self.regs.set(rd, val);
							let val = self.load_word(addr + 4).unwrap();
							self.regs.set(Register(rd.0 + 1), val);
						},
						MiscLoadStore::Doubleword { load: false } => {
							assert!(rd.0 % 2 == 0);
							self.store_word(addr, val).unwrap();
							let val = self.regs.get(Register(rd.0 + 1));
							self.store_word(addr + 4, val).unwrap();
						},
					};
					if walk == MiscWalk::Post {
						self.regs.set(ra, new_addr);
					};
				},
				instr => panic!("unhandled instr: {:?}", instr),
			}
		}
	}
}

pub mod nand {
	use mem;
	use binutil::{mask, bit};

	pub const BASE_ADDR: u32 = 0x4E00_0000;
	pub const CONTROL_ADDR: u32 = BASE_ADDR + 0x4;
	pub const START_BLOCK_ADDR: u32 = BASE_ADDR + 0x20;
	pub const END_BLOCK_ADDR: u32 = BASE_ADDR + 0x24;

	pub enum ECCDirection {
		Decode, Encode
	}

	pub struct Nand {
		start_block: u32,
		end_block: u32,
		ecc_direction: ECCDirection,
		lock_tight: bool,
		soft_lock: bool,
		enable_ecc_decode_complete_int: bool,
		ecc_8bit_init: bool,
		enable_illegal_access_int: bool,
		enable_rnb_int: bool,
	}
	impl Nand {
		pub fn new() -> Self {
			Self {
				start_block: 0,
				end_block: 0,
				ecc_direction: ECCDirection::Decode,
				lock_tight: false,
				soft_lock: true,
				enable_ecc_decode_complete_int: false,
				ecc_8bit_init: false,
				enable_illegal_access_int: false,
				enable_rnb_int: false,
			}
		}
	}
	impl mem::Store for Nand {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		// the docs list up to base + 0x64, I think it's a megabyte but it could be up to 8 megabytes
		fn max_addr(&self) -> u32 { BASE_ADDR + 0x64 + 0x3 }

		fn load_byte(&self, _: u32) -> Option<u8> { None }
		fn load_halfword(&self, _: u32) -> Option<u16> { None }
		fn load_word(&self, addr: u32) -> Option<u32> {
			match addr {
				CONTROL_ADDR => Some(
					match self.ecc_direction {
						ECCDirection::Decode => 0b0,
						ECCDirection::Encode => 0b1,
					} << 18 |
					bit(self.lock_tight) << 17 |
					bit(self.soft_lock) << 16 |
					bit(self.enable_ecc_decode_complete_int) << 12 |
					bit(self.ecc_8bit_init) << 11 |
					bit(self.enable_illegal_access_int) << 10 |
					bit(self.enable_rnb_int) << 9 |
					0
				),
				START_BLOCK_ADDR => Some(self.start_block),
				END_BLOCK_ADDR => Some(self.end_block),
				_ => None,
			}
		}

		fn store_byte(&mut self, _: u32, _: u8) -> bool { false }
		fn store_halfword(&mut self, _: u32, _: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				CONTROL_ADDR => {
					assert_eq!(data >> 19 & mask(13), 0);
					self.ecc_direction = match data >> 18 & 0b1 {
						0b0 => ECCDirection::Decode,
						0b1 => ECCDirection::Encode,
						_ => unreachable!(),
					};
					if !self.lock_tight {
						self.lock_tight = data >> 17 & 0b1 == 0b1;
					};
					self.soft_lock = data >> 16 & 0b1 == 0b1;
					assert_eq!(data >> 13 & mask(3), 0);
					self.enable_ecc_decode_complete_int = data >> 12 & 0b1 == 0b1;
					self.ecc_8bit_init = data >> 11 & 0b1 == 0b1;
					assert!(!self.ecc_8bit_init, "TODO");
					self.enable_illegal_access_int = data >> 10 & 0b1 == 0b1;
					self.enable_rnb_int = data >> 9 & 0b1 == 0b1;
					assert_eq!(data >> 0 & mask(9), 0, "TODO");
					true
				},
				START_BLOCK_ADDR => {
					self.start_block = data;
					true
				},
				END_BLOCK_ADDR => {
					self.end_block = data;
					true
				},
				_ => false,
			}
		}
	}
}

pub struct NullStore {
	pub len: u32,
}
impl NullStore {
	pub fn new(len: u32) -> Self {
		NullStore {
			len: len,
		}
	}
}
impl mem::Store for NullStore {
	fn min_addr(&self) -> u32 { 0 }
	fn max_addr(&self) -> u32 { self.len - 1 }

	fn load_byte(&self, _: u32) -> Option<u8> { Some(0) }
	fn load_halfword(&self, _: u32) -> Option<u16> { Some(0) }
	fn load_word(&self, _: u32) -> Option<u32> { Some(0) }

	fn store_byte(&mut self, _: u32, _: u8) -> bool { true }
	fn store_halfword(&mut self, _: u32, _: u16) -> bool { true }
	fn store_word(&mut self, _: u32, _: u32) -> bool { true }
}

pub mod gpio {
	pub const BASE_ADDR: u32 = 0x5600_0000;
	// the docs list up to base + 0x118, I think it's a megabyte but it could be up to 16 megabytes
	pub const SIZE: u32 = 0x118 + 0x3;
}

pub mod uart {
	pub const BASE_ADDR: u32 = 0x5000_0000;
	// the docs list up to base + 0xC02C, I think it's a megabyte but it could be up to 16 megabytes
	pub const SIZE: u32 = 0xC02C + 0x3;
}
