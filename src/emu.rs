extern crate byteorder;

use instr::*;
use instr;
use disasm;
use thumb_disasm;
use mem;
use std;
use binutil::{mask32, bit32};
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

	pub fn get_spsr_mut(&mut self) -> Option<&mut StatusRegister> {
		match self.cpsr.mode {
			Mode::User => None,
			Mode::FIQ => Some(&mut self.fiq.1),
			Mode::IRQ => Some(&mut self.irq.1),
			Mode::Supervisor => Some(&mut self.svc.1),
			Mode::Abort => Some(&mut self.abt.1),
			Mode::Undefined => Some(&mut self.und.1),
			Mode::System => None,
		}
	}

	pub fn get_status_register(&mut self, reg: instr::StatusRegister) -> Option<&StatusRegister> {
		match reg {
			instr::StatusRegister::Current => Some(&self.cpsr),
			instr::StatusRegister::Saved => self.get_spsr(),
		}
	}

	pub fn get_status_register_mut(&mut self, reg: instr::StatusRegister) -> Option<&mut StatusRegister> {
		match reg {
			instr::StatusRegister::Current => Some(&mut self.cpsr),
			instr::StatusRegister::Saved => self.get_spsr_mut(),
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

pub struct ControlRegister {
	mmu: bool,
	// alignment_fault: true,
	l1_cache: bool,
	// write_buffer: false,
	// SBO * 3
	// endianess: LE (0),
	s: bool,
	r: bool,
	f: bool,
	// program_flow_prediction: false,
	l1_instr_cache: bool,
	high_interrupts: bool,
	// predictable_cache_replacment: false,
	// L4: false,
	// DT: SBO,
	// SBZ
	// IT: SBO,
	// SBZ
	// ST: SBZ,
	// fast_interrupts: false,
	// U: false,
	// XP: false,
	// vectored_interrupts: false,
	// EE: false,
	// l2_cache: false,
}

fn mem_trace(min: u32, max: u32) {
	// if !(min > 0x31f6ffdf || max < 0x31f6ffdc) {
	// 	println!("HELLO!!!!!!!");
	// }
}

pub struct MMU {
	translation_table_addr: u32,
	fcse_pid: u8,
}

pub struct Config {
	pub nand: nand::Config,
}

pub struct Emulator {
	pub dram: mem::OffsetStore<mem::MmapStore>,
	pub sram: mem::OffsetStore<mem::MmapStore>,
	pub nand: nand::Nand,
	pub gpio: gpio::GPIO,
	pub uart: uart::UART,
	pub watchdog_timer: watchdog_timer::WatchdogTimer,
	pub interrupts: interrupts::Interrupts,
	pub pwm_timer: pwm_timer::PWMTimer,
	pub sys_con: sys_con::SysCon,
	pub dram_con: dram_con::DRAMCon,
	pub regs: Registers,
	pub control: ControlRegister,
	pub mmu: MMU,
}
impl Emulator {
	pub fn new(config: Config) -> Self {
		let psr = StatusRegister {
			negative: false,
			zero: true,
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
			nand: nand::Nand::new(config.nand),
			gpio: gpio::GPIO::new(),
			uart: uart::UART::new(),
			watchdog_timer: watchdog_timer::WatchdogTimer::new(),
			interrupts: interrupts::Interrupts::new(),
			pwm_timer: pwm_timer::PWMTimer::new(),
			sys_con: sys_con::SysCon::new(),
			dram_con: dram_con::DRAMCon::new(),
			regs: Registers {
				cpsr: psr.clone(),
				main: [0; 16],
				fiq: ([0; 7], psr.clone()),
				irq: ([0; 2], psr.clone()),
				svc: ([0; 2], psr.clone()),
				abt: ([0; 2], psr.clone()),
				und: ([0; 2], psr.clone()),
			},
			control: ControlRegister {
				mmu: false,
				l1_cache: false,
				s: false,
				r: false,
				f: false,
				l1_instr_cache: false,
				high_interrupts: false,
			},
			mmu: MMU {
				translation_table_addr: 0,
				fcse_pid: 0,
			},
		}
	}

	pub fn apply_fcse(&self, addr: u32) -> u32 {
		let mva = if addr >> 25 & mask32(7) == 0 {
			addr | (self.mmu.fcse_pid as u32) << 25
		} else {
			addr
		};
		// if self.mmu.fcse_pid != 0 {
		// 	println!("FCSE PID: {:x}", self.mmu.fcse_pid);
		// 	println!("VA {:x} -> MVA {:x}", addr, mva);
		// }
		mva
	}

	pub fn apply_mmu(&mut self, addr: u32) -> Result<u32> {
		let tmp = self.mmu.translation_table_addr | (addr >> 20 & mask32(12)) << 2;
		let entry_1 = self.load_word_phys(tmp)?;
		let phys_addr = match entry_1 & 0b11 {
			0b00 => panic!("TODO: memory fault: {:x}", addr),
			0b01 => { // coarse page table
				let entry_2 = self.load_word_phys(entry_1 & mask32(22) << 10 | (addr >> 12 & mask32(8)) << 2)?;
				match entry_2 & 0b11 {
					0b01 => { // large page
						entry_2 & mask32(16) << 16 | addr & mask32(16)
					},
					_ => unreachable!("{:02b}", entry_2 & 0b11),
				}
			},
			0b10 => { // section
				entry_1 & mask32(12) << 20 | addr & mask32(20)
			},
			_ => unreachable!("{:02b}", entry_1 & 0b11),
		};
		// println!("MVA {:x} -> PA {:x}", addr, phys_addr);
		Ok(phys_addr)
	}

	pub fn memories<'a>(&'a self) -> Vec<&'a mem::Store> { vec![
		&self.sram,
		&self.dram,
		&self.dram_con,
		&self.interrupts,
		&self.sys_con,
		&self.nand,
		&self.uart,
		&self.pwm_timer,
		&self.watchdog_timer,
		&self.gpio
	] }
	pub fn memories_mut<'a>(&'a mut self) -> Vec<&'a mut mem::Store> { vec![
		&mut self.sram,
		&mut self.dram,
		&mut self.dram_con,
		&mut self.interrupts,
		&mut self.sys_con,
		&mut self.nand,
		&mut self.uart,
		&mut self.pwm_timer,
		&mut self.watchdog_timer,
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

	pub fn load_byte_phys(&mut self, addr: u32) -> Result<u8> {
		Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr)?.load_byte(addr).ok_or(Error::Memory(addr, addr))
	}
	pub fn load_halfword_phys(&mut self, addr: u32) -> Result<u16> {
		assert!(addr & 0b1 == 0b0, "halfword alignment");
		Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr + 1)?.load_halfword(addr).ok_or(Error::Memory(addr, addr + 1))
	}
	pub fn load_word_phys(&mut self, addr: u32) -> Result<u32> {
		assert!(addr & 0b11 == 0b00, "word alignment");
		Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr + 3)?.load_word(addr).ok_or(Error::Memory(addr, addr + 3))
	}

	pub fn store_byte_phys(&mut self, addr: u32, data: u8) -> Result<()> {
		if Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr)?.store_byte(addr, data) {
			Ok(())
		} else {
			Err(Error::Memory(addr, addr))
		}
	}
	pub fn store_halfword_phys(&mut self, addr: u32, data: u16) -> Result<()> {
		assert!(addr & 0b1 == 0b0, "halfword alignment");
		if Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr + 1)?.store_halfword(addr, data) {
			Ok(())
		} else {
			Err(Error::Memory(addr, addr + 1))
		}
	}
	pub fn store_word_phys(&mut self, addr: u32, data: u32) -> Result<()> {
		assert!(addr & 0b11 == 0b00, "word alignment");
		if Self::find_store_mut(self.memories_mut().as_mut_slice(), addr, addr + 3)?.store_word(addr, data) {
			Ok(())
		} else {
			Err(Error::Memory(addr, addr + 3))
		}
	}

	pub fn load_byte(&mut self, addr: u32) -> Result<u8> {
		let addr = self.apply_fcse(addr);
		let addr = if self.control.mmu {
			self.apply_mmu(addr)?
		} else {
			addr
		};
		self.load_byte_phys(addr)
	}
	pub fn load_halfword(&mut self, addr: u32) -> Result<u16> {
		let addr = self.apply_fcse(addr);
		assert!(addr & 0b1 == 0b0, "halfword alignment");
		let addr = if self.control.mmu {
			self.apply_mmu(addr)?
		} else {
			addr
		};
		self.load_halfword_phys(addr)
	}
	pub fn load_word(&mut self, addr: u32) -> Result<u32> {
		let addr = self.apply_fcse(addr);
		assert!(addr & 0b11 == 0b00, "word alignment");
		let addr = if self.control.mmu {
			self.apply_mmu(addr)?
		} else {
			addr
		};
		self.load_word_phys(addr)
	}
	pub fn store_byte(&mut self, addr: u32, data: u8) -> Result<()> {
		let addr = self.apply_fcse(addr);
		let addr = if self.control.mmu {
			self.apply_mmu(addr)?
		} else {
			addr
		};
		self.store_byte_phys(addr, data)
	}
	pub fn store_halfword(&mut self, addr: u32, data: u16) -> Result<()> {
		let addr = self.apply_fcse(addr);
		assert!(addr & 0b1 == 0b0, "halfword alignment");
		let addr = if self.control.mmu {
			self.apply_mmu(addr)?
		} else {
			addr
		};
		self.store_halfword_phys(addr, data)
	}
	pub fn store_word(&mut self, addr: u32, data: u32) -> Result<()> {
		let addr = self.apply_fcse(addr);
		assert!(addr & 0b11 == 0b00, "word alignment");
		let addr = if self.control.mmu {
			self.apply_mmu(addr)?
		} else {
			addr
		};
		self.store_word_phys(addr, data)
	}

	pub fn get_reg_op(&self, reg: Register) -> u32 {
		if reg == Register(15) {
			match self.regs.cpsr.instruction_set {
				InstructionSet::ARM => self.regs.get(Register(15)) + 4,
				InstructionSet::Thumb => self.regs.get(Register(15)) + 2,
				InstructionSet::Jazelle => panic!("what are you doing with jazelle?"),
			}
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
				let shift = self.get_shift_op_shift(shift) & mask32(8);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else if shift <= 32 {
					base >> 32 - shift & 0b1 == 0b1
				} else {
					false
				}, if shift < 32 {
					base << shift
				} else {
					0
				})
			},
			ShiftOp::LSR(ShiftOpShift::Immediate(0)) => (base >> 31 & 0b1 == 0b1, 0), // unsigned base >> 32
			ShiftOp::LSR(shift) => {
				let shift = self.get_shift_op_shift(shift) & mask32(8);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else if shift <= 32 {
					base >> shift - 1 & 0b1 == 0b1
				} else {
					false
				}, if shift < 32 {
					base >> shift
				} else {
					0
				})
			},
			ShiftOp::ASR(ShiftOpShift::Immediate(0)) => if base >> 31 & 0b1 == 0b1 { // signed base >> 32
				(true, u32::max_value())
			} else {
				(false, 0)
			},
			ShiftOp::ASR(shift) => {
				let shift = self.get_shift_op_shift(shift) & mask32(8);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else if shift <= 32 {
					base >> shift - 1 & 0b1 == 0b1
				} else {
					base >> 31 & 0b1 == 0b1
				}, (base as i32 >> shift) as u32)
			},
			ShiftOp::ROR(shift) => {
				let shift = self.get_shift_op_shift(shift) & mask32(8);
				let res = base.rotate_right(shift);
				(if shift == 0 {
					self.regs.cpsr.carry
				} else {
					res >> 31 & 0b1 == 0b1
				}, res)
			},
			ShiftOp::RRX => {
				(base & 0b1 == 0b1, base >> 1 | bit32(self.regs.cpsr.carry) << 31)
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

	pub fn put_mul_dest<N: Debug, A: Debug, SA: Debug, SS: Debug, LN: Debug, LA: Debug>(&mut self, dest: MulDest<N, A, SA, SS, LN, LA>, val: u64, update_status: bool) {
		match dest {
			MulDest::SeparateAccumulate(_, rd, ra) => {
				let a = self.get_reg_op(ra);
				self.regs.set(rd, (val + a as u64) as u32);
				if update_status {
					self.regs.cpsr.negative = val >> 31 & 0b1 == 0b1;
					self.regs.cpsr.zero = val as u32 == 0;
				}
			},
			MulDest::LongNormal { is: _, lo, hi } => {
				self.regs.set(lo, val as u32);
				self.regs.set(hi, (val >> 32) as u32);
				if update_status {
					self.regs.cpsr.negative = val >> 63 & 0b1 == 0b1;
					self.regs.cpsr.zero = val == 0;
				}
			},
			MulDest::Normal(_, rd) => {
				self.regs.set(rd, val as u32);
				if update_status {
					self.regs.cpsr.negative = val >> 31 & 0b1 == 0b1;
					self.regs.cpsr.zero = val as u32 == 0;
				}
			},
			_ => panic!("unhandled mul dest: {:?}", dest),
		}
	}

	pub fn get_store_status_op(&self, op: StoreStatusOp) -> u32 {
		match op {
			StoreStatusOp::Register(r) => self.get_reg_op(r),
			StoreStatusOp::Immediate { imm, rotate } => (imm as u32).rotate_right(rotate as u32 * 2),
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
			Condition::UnsignedGT => self.regs.cpsr.carry && !self.regs.cpsr.zero,
			Condition::UnsignedLTE => !self.regs.cpsr.carry || self.regs.cpsr.zero,
			Condition::SignedGTE => self.regs.cpsr.negative == self.regs.cpsr.overflow,
			Condition::SignedLT => self.regs.cpsr.negative != self.regs.cpsr.overflow,
			Condition::SignedGT => !self.regs.cpsr.zero && self.regs.cpsr.negative == self.regs.cpsr.overflow,
			Condition::SignedLTE => self.regs.cpsr.zero || self.regs.cpsr.negative != self.regs.cpsr.overflow,
			Condition::Always => true,
			Condition::Custom => true,
		}
	}

	pub fn tick(&mut self) {
		let pc = self.regs.main[15];
		let instr = match self.regs.cpsr.instruction_set {
			InstructionSet::ARM => {
				assert!(pc & 0b11 == 0, "PC must be word aligned");
				let instr_raw = self.load_word(pc).unwrap();
				let instr = disasm::disasm_instr(instr_raw);
				self.regs.main[15] += 4;
				instr
			},
			InstructionSet::Thumb => {
				assert!(self.regs.main[15] & 0b1 == 0, "PC must be halfword aligned");
				let instr = thumb_disasm::disasm_instr(|| {
					let tmp = self.regs.main[15];
					let val = self.load_halfword(tmp).unwrap();
					// println!("thumb read: {:x} = {:016b}", self.regs.main[15], val);
					self.regs.main[15] += 2;
					val
				}).unwrap();
				instr
			},
			instruction_set => panic!("{:?} isn't supported", instruction_set),
		};
		// println!("{:x} {}", pc, instr.pp(pc));
		// println!("{:x}", pc);
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
							dest_used = true;
							self.regs.set(dest, out);
						},
						DataOp::Negate => {
							let (carry_, out_) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = !out_;
							dest_used = true;
							self.regs.set(dest, out);
						},
						DataOp::Compare => {
							let a1 = self.get_reg_op(op1);
							let (_, a2) = self.get_shift(op2);
							let (out_, carry_) = a1.overflowing_sub(a2);
							carry = !carry_;
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
						DataOp::TestEq => {
							let a1 = self.get_reg_op(op1);
							let (carry_, a2) = self.get_shift(op2);
							out = a1 ^ a2;
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
							self.regs.set(dest, out);
						},
						DataOp::XOr => {
							let (carry_, a2) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = self.get_reg_op(op1) ^ a2;
							dest_used = true;
							self.regs.set(dest, out);
						},
						DataOp::And => {
							let (carry_, a2) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = self.get_reg_op(op1) & a2;
							dest_used = true;
							self.regs.set(dest, out);
						},
						DataOp::BitClear => {
							let a1 = self.get_reg_op(op1);
							let (carry_, a2) = self.get_shift(op2);
							carry = carry_;
							overflow = None;
							out = a1 & !a2;
							dest_used = true;
							self.regs.set(dest, out);
						},
						DataOp::Add { carry: use_carry } => {
							let a1 = self.get_reg_op(op1);
							let (_, a2) = self.get_shift(op2);
							let (out_, carry_) = a1.overflowing_add(a2);
							if use_carry && self.regs.cpsr.carry {
								let (out__, carry__) = out_.overflowing_add(1);
								out = out__;
								carry = carry_ || carry__;
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
								carry = !carry_;
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
					mem_trace(addr, addr + 0x3);
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
					let val = {
						let reg = self.regs.get_status_register(src).unwrap();
						let (j, t) = match reg.instruction_set {
							InstructionSet::ARM => (false, false),
							InstructionSet::Thumb => (false, true),
							InstructionSet::Jazelle => (true, false),
						};
						bit32(reg.negative) << 31 |
						bit32(reg.zero) << 30 |
						bit32(reg.carry) << 29 |
						bit32(reg.overflow) << 28 |
						bit32(reg.dsp_overflow) << 27 |
						bit32(j) << 24 |
						bit32(reg.disable_irq) << 7 |
						bit32(reg.disable_fiq) << 6 |
						bit32(t) << 5 |
						match reg.mode {
							Mode::User       => 0b10000,
							Mode::FIQ        => 0b10001,
							Mode::IRQ        => 0b10010,
							Mode::Supervisor => 0b10011,
							Mode::Abort      => 0b10111,
							Mode::Undefined  => 0b11011,
							Mode::System     => 0b11111,
						} << 0 |
						0
					};
					self.regs.set(dst, val);
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
					mem_trace(min_addr, min_addr + num * 0x4 - 0x1);
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
				Instr::ThumbyJump { cond: _, link, addr: ra } => {
					let addr = self.get_reg_op(ra);
					if addr & 0b1 == 0b1 {
						self.regs.cpsr.instruction_set = InstructionSet::Thumb;
					} else {
						self.regs.cpsr.instruction_set = InstructionSet::ARM;
					}
					if link {
						let val = self.regs.get(Register(15));
						self.regs.set(Register(14), val)
					}
					self.regs.set(Register(15), addr & !(1 << 0));
				},
				Instr::ThumbJump { cond: _, addr } => {
					let val = self.regs.get(Register(15)).wrapping_add(2).wrapping_add((addr as u32) << 1);
					self.regs.set(Register(15), val);
				},
				Instr::ThumbJumpPrefix { offset } => {
					let addr = self.regs.get(Register(15)).wrapping_add(((offset as i32).wrapping_shl(21).wrapping_shr(21) << 12) as u32);
					self.regs.set(Register(14), addr)
				},
				Instr::ThumbCall { offset, exchange } => {
					let addr = self.regs.get(Register(14)).wrapping_add((offset << 1) as u32);
					let addr = if exchange {
						addr & !0b11
					} else {
						addr
					};
					let link = self.regs.get(Register(15));
					self.regs.set(Register(14), link);
					self.regs.set(Register(15), addr);
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
							} & mask32(16);
							let op2 = if op2_hi {
								op2 >> 16
							} else {
								op2
							} & mask32(16);
							self.put_mul_dest(dest, op1 as u64 * op2 as u64, false);
						},
						MulOp::Long { dest, signed, update_status } => {
							let val = if signed {
								(op1 as i32 as i64 * op2 as i32 as i64) as u64
							} else {
								op1 as u64 * op2 as u64
							};
							self.put_mul_dest(dest, val, update_status);
						},
						MulOp::Lo32 { dest, update_status } => {
							self.put_mul_dest(dest, op1 as u64 * op2 as u64, update_status);
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
					mem_trace(addr, addr + 0x7);
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
				Instr::StoreStatus { cond: _, dst: dst_r, src, mask_c, mask_x, mask_s, mask_f } => {
					let val = self.get_store_status_op(src);
					let dst = self.regs.get_status_register_mut(dst_r).unwrap();
					if dst_r == instr::StatusRegister::Current && (val >> 5 & 0b1 == 0b1 || val >> 24 & 0b1 == 0b1) {
						panic!("attempt to switch execution mode using MSR");
					}
					let (mut j, mut t) = match dst.instruction_set {
						InstructionSet::ARM => (false, false),
						InstructionSet::Thumb => (false, true),
						InstructionSet::Jazelle => (true, false),
					};
					if mask_c {
						dst.disable_irq = val >> 7 & 0b1 == 0b1;
						dst.disable_fiq = val >> 6 & 0b1 == 0b1;
						t = val >> 5 & 0b1 == 0b1;
						dst.mode = match val >> 0 & mask32(5) {
							0b10000 => Mode::User,
							0b10001 => Mode::FIQ,
							0b10010 => Mode::IRQ,
							0b10011 => Mode::Supervisor,
							0b10111 => Mode::Abort,
							0b11011 => Mode::Undefined,
							0b11111 => Mode::System,
							v => panic!("unknown mode: {:b}", v),
						};
					}
					if mask_x {
						// nothing here for ARMv5
					}
					if mask_s {
						// nothing here for ARMv5
					}
					if mask_f {
						dst.negative = val >> 31 & 0b1 == 0b1;
						dst.zero = val >> 30 & 0b1 == 0b1;
						dst.carry = val >> 29 & 0b1 == 0b1;
						dst.overflow = val >> 28 & 0b1 == 0b1;
						dst.dsp_overflow = val >> 27 & 0b1 == 0b1;
						j = val >> 24 & 0b1 == 0b1;
					}
					dst.instruction_set = match (j, t) {
						(false, false) => InstructionSet::ARM,
						(false, true) => InstructionSet::Thumb,
						(true, false) => InstructionSet::Jazelle,
						(true, true) => panic!("J = 1 ∧ T = 1 is reserved"),
					}
				},
				Instr::Coprocessor { cond: _, op, coproc, opcode1, opcode2, reg, creg1, creg2 } => {
					match (op, coproc, opcode1, opcode2, reg, creg1, creg2) {
						(Some(CoprocessorTransfer::From), 15, 0, 0, reg, Register(1), creg2) => {
							let val =
								bit32(self.control.mmu) << 0 |
								/* alignment_fault */ 1 << 1 |
								bit32(self.control.l1_cache) << 2 |
								/* write_buffer */ 0 << 3 |
								1 << 4 | 1 << 5 | 1 << 6 |
								/* endianess */ 0 << 7 |
								bit32(self.control.s) << 8 |
								bit32(self.control.r) << 9 |
								bit32(self.control.f) << 10 |
								/* program_flow_prediction */ 0 << 11 |
								bit32(self.control.l1_instr_cache) << 12 |
								bit32(self.control.high_interrupts) << 13 |
								/* predictable_cache_replacment */ 0 << 14 |
								/* L4 */ 0 << 15 |
								/* DT (SBO) */ 1 << 16 |
								0 << 17 |
								/* IT (SBO) */ 1 << 18 |
								0 << 19 |
								/* ST (SBZ) */ 0 << 20 |
								/* fast_interrupts */ 0 << 21 |
								/* U */ 0 << 22 |
								/* XP */ 0 << 23 |
								/* vectored_interrupts */ 0 << 24 |
								/* EE */ 0 << 25 |
								/* l2_cache */ 0 << 26 |
								0;
							self.regs.set(reg, val)
						},
						(Some(CoprocessorTransfer::To), 15, 0, 0, reg, Register(1), creg2) => {
							let val = self.get_reg_op(reg);
							self.control.mmu = val >> 0 & 0b1 == 0b1;
							assert_eq!(val >> 1 & 0b1, 1); // alignment_fault
							self.control.l1_cache = val >> 2 & 0b1 == 0b1;
							assert_eq!(val >> 3 & 0b1, 0); // write_buffer
							assert_eq!(val >> 7 & 0b1, 0); // endianess
							self.control.s = val >> 8 & 0b1 == 0b1;
							self.control.r = val >> 9 & 0b1 == 0b1;
							self.control.f = val >> 10 & 0b1 == 0b1;
							assert_eq!(val >> 11 & 0b1, 0); // program_flow_prediction
							self.control.l1_instr_cache = val >> 12 & 0b1 == 0b1;
							self.control.high_interrupts = val >> 13 & 0b1 == 0b1;
							assert_eq!(val >> 14 & 0b1, 0); // predictable_cache_replacment
							assert_eq!(val >> 15 & 0b1, 0); // L4
							assert_eq!(val >> 21 & 0b1, 0); // fast_interrupts
							assert_eq!(val >> 22 & 0b1, 0); // U
							assert_eq!(val >> 23 & 0b1, 0); // XP
							assert_eq!(val >> 24 & 0b1, 0); // vectored_interrupts
							assert_eq!(val >> 25 & 0b1, 0); // EE
							assert_eq!(val >> 26 & 0b1, 0); // l2_cache
						},
						(_, 15, 0, 0, _, Register(7), Register(5)) => {}, // invalidate entire instruction cache
						(_, 15, 0, 2, _, Register(7), Register(14)) => {}, // clean and invalidate cache line
						(Some(CoprocessorTransfer::To), 15, 0, 0, _, Register(8), Register(7)) => {}, // invalidate entire TLBs
						(Some(CoprocessorTransfer::To), 15, 0, 0, _, Register(8), Register(7)) => {}, // invalidate entire TLBs
						(Some(CoprocessorTransfer::To), 15, 0, 0, reg, Register(2), Register(0)) => {
							self.mmu.translation_table_addr = self.get_reg_op(reg) & mask32(18) << 14;
						},
						(Some(CoprocessorTransfer::To), 15, 0, 0, _, Register(3), Register(0)) => {}, // domain access control TODO
						(Some(CoprocessorTransfer::To), 15, 0, 0, _, Register(13), Register(0)) => {
							self.mmu.fcse_pid = (self.get_reg_op(reg) >> 25) as u8;
						},
						_ => panic!("unhandled coprocessor thing: op = {:?}, coproc = {}, opcode1 = {}, opcode2 = {}, creg1 = {:?}, creg2 = {:?}", op, coproc, opcode1, opcode2, creg1, creg2),
					}
				},
				Instr::CountLeadingZeros { cond: _, dest, src } => {
					let val = self.get_reg_op(src).leading_zeros();
					self.regs.set(dest, val);
				},
				instr => panic!("unhandled instr: {:?}", instr),
			}
		}
	}
}

pub mod nand {
	use mem;
	use binutil::{mask64, mask32, bit32};

	pub const BASE_ADDR: u32 = 0x4E00_0000;
	pub const CONFIG_ADDR: u32 = BASE_ADDR;
	pub const CONTROL_ADDR: u32 = BASE_ADDR + 0x4;
	pub const COMMAND_ADDR: u32 = BASE_ADDR + 0x8;
	pub const ADDRESS_ADDR: u32 = BASE_ADDR + 0xc;
	pub const DATA_ADDR: u32 = BASE_ADDR + 0x10;
	pub const START_BLOCK_ADDR: u32 = BASE_ADDR + 0x20;
	pub const END_BLOCK_ADDR: u32 = BASE_ADDR + 0x24;
	pub const STATUS_ADDR: u32 = BASE_ADDR + 0x28;
	pub const ERROR_0_ADDR: u32 = BASE_ADDR + 0x2c;
	pub const ERROR_1_ADDR: u32 = BASE_ADDR + 0x30;

	pub const BLOCK_SIZE: u32 = 128 * 1024;
	pub const PAGE_SIZE: u32 = 2048;

	pub enum ECCDirection {
		Decode, Encode
	}

	pub enum MsgLength {
		L512,
		L24,
	}

	#[derive(Debug)]
	pub enum State {
		Normal,
		ReadAddr {
			byte: u32,
			addr: u64,
		},
		Read {
			byte: u32,
			addr: (u16, u8, u16),
		},
		ReadId(Option<(u8, u8)>),
		ChangeReadAddr {
			byte: u32,
			block: u16,
			page: u8,
			column: u16,
		},
	}

	fn parse_addr(addr: u64) -> (u16, u8, u16) {
		assert_eq!(addr >> 12 & mask64(4), 0);
		(
			(addr >> 22 & mask64(13)) as u16,
			(addr >> 16 & mask64(6)) as u8,
			(addr & mask64(12)) as u16
		)
	}

	pub struct Config {
		pub store: Box<mem::Store>,
	}

	pub struct Nand {
		state: State,
		store: Box<mem::Store>,

		start_block: u32,
		end_block: u32,

		ecc_direction: ECCDirection,
		lock_tight: bool,
		soft_lock: bool,
		enable_ecc_decode_complete_int: bool,
		ecc_8bit_init: bool,
		enable_illegal_access_int: bool,
		enable_rnb_trans_int: bool,
		rnb_trans_falling: bool,
		chip_1_select: bool,
		chip_enable: bool,
		enable: bool,

		msg_length: MsgLength,
		ecc_type: u8, // 2 bit
		tacls: u8, // 3 bit
		twrph0: u8, // 3 bit
		twrph1: u8, // 3 bit
		page_size: bool,
		page_size_ext: bool,
		addr_cycle: bool,

		ecc_decoding_done: bool,
		illegal_access: bool,
		rnb_trans_detected: bool,
	}
	impl Nand {
		pub fn new(config: Config) -> Self {
			Self {
				state: State::Normal,
				store: config.store,

				start_block: 0,
				end_block: 0,

				ecc_direction: ECCDirection::Decode,
				lock_tight: false,
				soft_lock: true,
				enable_ecc_decode_complete_int: false,
				ecc_8bit_init: false,
				enable_illegal_access_int: false,
				enable_rnb_trans_int: false,
				rnb_trans_falling: false,
				chip_1_select: true,
				chip_enable: true,
				enable: false,

				msg_length: MsgLength::L512,
				ecc_type: 0,
				tacls: 1,
				twrph0: 0,
				twrph1: 0,
				page_size: true,
				page_size_ext: true,
				addr_cycle: true,

				ecc_decoding_done: false,
				illegal_access: false,
				rnb_trans_detected: true,
			}
		}
	}
	impl mem::Store for Nand {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		// the docs list up to base + 0x64, I think it's a megabyte but it could be up to 8 megabytes
		fn max_addr(&self) -> u32 { BASE_ADDR + 0x64 + 0x3 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> {
			match addr {
				DATA_ADDR => {
					println!("NAND data read in state {:?}", self.state);
					match self.state {
						State::ReadId(Some((0x00, ref mut part))) => {
							let val = match *part {
								0 => 0xad, // manufacturer id
								1 => 0xda, // device id
								2 =>
									0b00 << 0 | // internal chip number = 1
									0b00 << 2 | // cell type = 2 level
									0b01 << 4 | // number of simultaneously programmed pages = 2
									0 << 6 | // interleave program between multiple chips = not supported
									0 << 7 | // cache program = not supported
									0,
								3 =>
									0b01 << 0 | // page size = 2KB
									0b01 << 4 | // block size = 128KB
									0b1 << 2 | // redundant area size = 16 bytes per 512 bytes
									0b0 << 6 | // organization = x8
									0b1 << 7 | 0b0 << 3 | // serial access minimum = 25ns
									0,
								4 =>
									0b01 << 2 | // plane number = 2
									0b100 << 4 | // plane size = 1 Gb
									0,
								_ => panic!("no more NAND ID data: {}", part),
							};
							*part += 1;
							Some(val)
						},
						State::Read { byte, addr: (block, page, column) } => {
							let addr = block as u32 * BLOCK_SIZE + page as u32 * PAGE_SIZE + column as u32 + byte;
							println!("NAND read {:x}", addr);
							self.state = State::Read { byte: byte + 1, addr: (block, page, column) };
							self.ecc_decoding_done = true;
							Some(if addr < self.store.min_addr() || addr > self.store.max_addr() {
								0
							} else {
								self.store.load_byte(addr).unwrap_or(0)
							})
						},
						_ => panic!("NAND data read in state {:?}", self.state),
					}
				},
				_ => None,
			}
		}
		fn load_halfword(&mut self, _: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			match addr {
				CONFIG_ADDR => Some(
					match self.msg_length {
						MsgLength::L512 => 0b0,
						MsgLength::L24 => 0b1,
					} << 25 |
					(self.ecc_type as u32) << 23 |
					(self.tacls as u32) << 12 |
					(self.twrph0 as u32) << 8 |
					(self.twrph1 as u32) << 4 |
					bit32(self.page_size) << 3 |
					bit32(self.page_size_ext) << 2 |
					bit32(self.addr_cycle) << 1 |
					/* bus width = 8 bit */ 0b0 << 0 |
					0
				),
				CONTROL_ADDR => Some(
					match self.ecc_direction {
						ECCDirection::Decode => 0b0,
						ECCDirection::Encode => 0b1,
					} << 18 |
					bit32(self.lock_tight) << 17 |
					bit32(self.soft_lock) << 16 |
					bit32(self.enable_ecc_decode_complete_int) << 12 |
					bit32(self.ecc_8bit_init) << 11 |
					bit32(self.enable_illegal_access_int) << 10 |
					bit32(self.enable_rnb_trans_int) << 9 |
					bit32(self.rnb_trans_falling) << 8 |
					bit32(self.chip_1_select) << 2 |
					bit32(self.chip_enable) << 1 |
					0
				),
				START_BLOCK_ADDR => Some(self.start_block),
				END_BLOCK_ADDR => Some(self.end_block),
				STATUS_ADDR => Some(
					bit32(self.ecc_decoding_done) << 6 |
					bit32(self.illegal_access) << 5 |
					bit32(self.rnb_trans_detected) << 4 |
					bit32(self.chip_1_select) << 3 |
					bit32(self.chip_enable) << 2 |
					/* RnB = ready */ 1 << 0 |
					0
				),
				ERROR_0_ADDR => Some(
					match self.ecc_type {
						0b10 => 1 << 30,
						ecc_type => panic!("ecc error 0 for ecc type {:02b}", ecc_type),
					}
				),
				_ => None,
			}
		}

		fn store_byte(&mut self, _: u32, _: u8) -> bool { false }
		fn store_halfword(&mut self, _: u32, _: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				CONFIG_ADDR => {
					self.msg_length = match data >> 25 & 0b1 {
						0b0 => MsgLength::L512,
						0b1 => MsgLength::L24,
						_ => unreachable!(),
					};
					self.ecc_type = (data >> 23 & 0b11) as u8;
					self.tacls = (data >> 12 & 0b111) as u8;
					self.twrph0 = (data >> 8 & 0b111) as u8;
					self.twrph1 = (data >> 4 & 0b111) as u8;
					self.page_size = data >> 3 & 0b1 == 0b1;
					self.page_size_ext = data >> 2 & 0b1 == 0b1;
					self.addr_cycle = data >> 1 & 0b1 == 0b1;
					assert_eq!(data >> 0 & 0b1, 0b0, "bus width");
					true
				},
				CONTROL_ADDR => {
					assert_eq!(data >> 19 & mask32(13), 0);
					self.ecc_direction = match data >> 18 & 0b1 {
						0b0 => ECCDirection::Decode,
						0b1 => ECCDirection::Encode,
						_ => unreachable!(),
					};
					if !self.lock_tight {
						self.lock_tight = data >> 17 & 0b1 == 0b1;
					};
					self.soft_lock = data >> 16 & 0b1 == 0b1;
					assert_eq!(data >> 13 & mask32(3), 0);
					self.enable_ecc_decode_complete_int = data >> 12 & 0b1 == 0b1;
					self.ecc_8bit_init = data >> 11 & 0b1 == 0b1;
					assert!(!self.ecc_8bit_init, "TODO");
					self.enable_illegal_access_int = data >> 10 & 0b1 == 0b1;
					self.enable_rnb_trans_int = data >> 9 & 0b1 == 0b1;
					self.rnb_trans_falling = data >> 8 & 0b1 == 0b1;
					self.chip_1_select = data >> 2 & 0b1 == 0b1;
					self.chip_enable = data >> 1 & 0b1 == 0b1;
					if self.chip_enable {
						self.state = State::Normal;
					}
					self.enable = data >> 0 & 0b1 == 0b1;
					true
				},
				COMMAND_ADDR => {
					println!("NAND command {:02x} in state {:?}", data, self.state);
					match (&self.state, data) {
						(&State::Normal, 0xff) => { // reset
							self.rnb_trans_detected = true;
						},
						(&State::Normal, 0x00) => {
							self.state = State::ReadAddr { byte: 0, addr: 0 };
						},
						(&State::Normal, 0x90) => {
							self.state = State::ReadId(None);
						},
						(&State::ReadAddr { byte: 5, addr }, 0x30) => {
							self.state = State::Read { byte: 0, addr: parse_addr(addr) };
							self.rnb_trans_detected = true;
						},
						(&State::Read { addr: (block, page, _), .. }, 0x05) => {
							self.state = State::ChangeReadAddr {
								byte: 0,
								block: block,
								page: page,
								column: 0,
							};
						},
						(&State::ChangeReadAddr { byte: 2, block, page, column }, 0xe0) => {
							self.state = State::Read {
								byte: 0,
								addr: (block, page, column),
							};
						},
						_ => panic!("NAND command {:02x} in state {:?}", data, self.state),
					}
					true
				},
				ADDRESS_ADDR => {
					println!("NAND address {:02x} in state {:?}", data, self.state);
					match self.state {
						State::ReadId(None) => {
							self.state = State::ReadId(Some((data as u8, 0)));
						},
						State::ReadAddr { ref mut byte, ref mut addr } => {
							if *byte == 5 {
								panic!("are you sure?");
							}
							*addr |= (data as u64) << *byte * 8;
							*byte += 1;
						},
						State::ChangeReadAddr { ref mut byte, ref mut column, .. } => {
							if *byte == 2 {
								panic!("are you sure?");
							}
							*column |= (data as u16) << *byte * 8;
							*byte += 1;
						},
						_ => panic!("NAND address {:02x} in state {:?}", data, self.state),
					}
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
				STATUS_ADDR => {
					if data >> 6 & 0b1 == 0b1 {
						self.ecc_decoding_done = false;
					}
					self.illegal_access = data >> 5 & 0b1 == 0b1;
					if data >> 4 & 0b1 == 0b1 {
						self.rnb_trans_detected = false;
					}
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

	fn load_byte(&mut self, _: u32) -> Option<u8> { Some(0) }
	fn load_halfword(&mut self, _: u32) -> Option<u16> { Some(0) }
	fn load_word(&mut self, _: u32) -> Option<u32> { Some(0) }

	fn store_byte(&mut self, _: u32, _: u8) -> bool { true }
	fn store_halfword(&mut self, _: u32, _: u16) -> bool { true }
	fn store_word(&mut self, _: u32, _: u32) -> bool { true }
}

pub mod gpio {
	use mem;
	use binutil::{bit32, mask32};

	pub const CONTROL_OFFSET: u32 = 0x0;
	pub const DATA_OFFSET: u32 = 0x4;
	pub const PULL_OFFSET: u32 = 0x8;
	pub const SELECT_OFFSET: u32 = 0xc;

	pub const BASE_ADDR: u32 = 0x5600_0000;

	pub const PORT_A_ADDR: u32 = BASE_ADDR;
	pub const PORT_A_CONTROL_ADDR: u32 = PORT_A_ADDR + CONTROL_OFFSET;
	pub const PORT_A_DATA_ADDR:    u32 = PORT_A_ADDR + DATA_OFFSET;

	pub const PORT_B_ADDR: u32 = BASE_ADDR + 0x10;
	pub const PORT_B_CONTROL_ADDR: u32 = PORT_B_ADDR + CONTROL_OFFSET;
	pub const PORT_B_DATA_ADDR:    u32 = PORT_B_ADDR + DATA_OFFSET;
	pub const PORT_B_PULL_ADDR:    u32 = PORT_B_ADDR + PULL_OFFSET;
	pub const PORT_B_SELECT_ADDR:  u32 = PORT_B_ADDR + SELECT_OFFSET;

	pub const PORT_C_ADDR: u32 = BASE_ADDR + 0x20;
	pub const PORT_C_CONTROL_ADDR: u32 = PORT_C_ADDR + CONTROL_OFFSET;
	pub const PORT_C_DATA_ADDR:    u32 = PORT_C_ADDR + DATA_OFFSET;
	pub const PORT_C_PULL_ADDR:    u32 = PORT_C_ADDR + PULL_OFFSET;

	pub const PORT_D_ADDR: u32 = BASE_ADDR + 0x30;
	pub const PORT_D_CONTROL_ADDR: u32 = PORT_D_ADDR + CONTROL_OFFSET;
	pub const PORT_D_DATA_ADDR:    u32 = PORT_D_ADDR + DATA_OFFSET;
	pub const PORT_D_PULL_ADDR:    u32 = PORT_D_ADDR + PULL_OFFSET;

	pub const PORT_E_ADDR: u32 = BASE_ADDR + 0x40;
	pub const PORT_E_CONTROL_ADDR: u32 = PORT_E_ADDR + CONTROL_OFFSET;
	pub const PORT_E_DATA_ADDR:    u32 = PORT_E_ADDR + DATA_OFFSET;
	pub const PORT_E_PULL_ADDR:    u32 = PORT_E_ADDR + PULL_OFFSET;
	pub const PORT_E_SELECT_ADDR:  u32 = PORT_E_ADDR + SELECT_OFFSET;

	pub const PORT_F_ADDR: u32 = BASE_ADDR + 0x50;
	pub const PORT_F_CONTROL_ADDR: u32 = PORT_F_ADDR + CONTROL_OFFSET;
	pub const PORT_F_DATA_ADDR:    u32 = PORT_F_ADDR + DATA_OFFSET;
	pub const PORT_F_PULL_ADDR:    u32 = PORT_F_ADDR + PULL_OFFSET;

	pub const PORT_G_ADDR: u32 = BASE_ADDR + 0x60;
	pub const PORT_G_CONTROL_ADDR: u32 = PORT_G_ADDR + CONTROL_OFFSET;
	pub const PORT_G_DATA_ADDR:    u32 = PORT_G_ADDR + DATA_OFFSET;
	pub const PORT_G_PULL_ADDR:    u32 = PORT_G_ADDR + PULL_OFFSET;

	pub const PORT_H_ADDR: u32 = BASE_ADDR + 0x70;
	pub const PORT_H_CONTROL_ADDR: u32 = PORT_H_ADDR + CONTROL_OFFSET;
	pub const PORT_H_DATA_ADDR:    u32 = PORT_H_ADDR + DATA_OFFSET;
	pub const PORT_H_PULL_ADDR:    u32 = PORT_H_ADDR + PULL_OFFSET;

	pub const PORT_K_ADDR: u32 = BASE_ADDR + 0xe0;
	pub const PORT_K_CONTROL_ADDR: u32 = PORT_K_ADDR + CONTROL_OFFSET;
	pub const PORT_K_DATA_ADDR:    u32 = PORT_K_ADDR + DATA_OFFSET;
	pub const PORT_K_PULL_ADDR:    u32 = PORT_K_ADDR + PULL_OFFSET;

	pub const PORT_L_ADDR: u32 = BASE_ADDR + 0xf0;
	pub const PORT_L_CONTROL_ADDR: u32 = PORT_L_ADDR + CONTROL_OFFSET;
	pub const PORT_L_DATA_ADDR:    u32 = PORT_L_ADDR + DATA_OFFSET;
	pub const PORT_L_PULL_ADDR:    u32 = PORT_L_ADDR + PULL_OFFSET;
	pub const PORT_L_SELECT_ADDR:    u32 = PORT_L_ADDR + SELECT_OFFSET;

	pub const PORT_M_ADDR: u32 = BASE_ADDR + 0x100;
	pub const PORT_M_CONTROL_ADDR: u32 = PORT_M_ADDR + CONTROL_OFFSET;
	pub const PORT_M_DATA_ADDR:    u32 = PORT_M_ADDR + DATA_OFFSET;
	pub const PORT_M_PULL_ADDR:    u32 = PORT_M_ADDR + PULL_OFFSET;

	pub const EINT_MASK_ADDR: u32 = BASE_ADDR + 0xa4;
	pub const EINT_PEND_ADDR: u32 = BASE_ADDR + 0xa8;

	pub const DRIVE_STRENGTH_0_ADDR: u32 = BASE_ADDR + 0xc0;
	pub const DRIVE_STRENGTH_1_ADDR: u32 = BASE_ADDR + 0xc4;
	pub const DRIVE_STRENGTH_2_ADDR: u32 = BASE_ADDR + 0xc8;
	pub const DRIVE_STRENGTH_3_ADDR: u32 = BASE_ADDR + 0x110;

	pub const STATUS_2_ADDR: u32 = BASE_ADDR + 0xb0;
	// the docs list up to base + 0x118, I think it's a megabyte but it could be up to 16 megabytes
	pub const SIZE: u32 = 0x118 + 0x4;

	pub const SOFTWARE_PLATFORM_ID: u32 = 0x32450003;

	pub struct GPIO {
		eint_mask: u32,
		eint_pend: u32,

		drive_strength_0: u32,
		drive_strength_1: u32,
		drive_strength_2: u32,
		drive_strength_3: u32,

		port_a_control: u32,
		port_a_data: u32,

		port_b_control: u32,
		port_b_data: u32,
		port_b_pull: u32,
		port_b_select: u32,

		port_c_control: u32,
		port_c_data: u32,
		port_c_pull: u32,

		port_d_control: u32,
		port_d_data: u32,
		port_d_pull: u32,

		port_e_control: u32,
		port_e_data: u32,
		port_e_pull: u32,
		port_e_select: u32,

		port_f_control: u32,
		port_f_data: u32,
		port_f_pull: u32,

		port_g_control: u32,
		port_g_data: u32,
		port_g_pull: u32,

		port_h_control: u32,
		port_h_data: u32,
		port_h_pull: u32,

		port_k_control: u32,
		port_k_data: u32,
		port_k_pull: u32,

		port_l_control: u32,
		port_l_data: u32,
		port_l_pull: u32,
		port_l_select: u32,

		port_m_control: u32,
		port_m_data: u32,
		port_m_pull: u32,
	}
	impl GPIO {
		pub fn new() -> Self {
			Self {
				eint_mask: u32::max_value(),
				eint_pend: 0,

				drive_strength_0: 0x2aaa_aaaa,
				drive_strength_1: 0xaaa_aaaa,
				drive_strength_2: 0xaa8_aaaa,
				drive_strength_3: 0x2aa,

				port_a_control: 0x0fffffff,
				port_a_data: 0,

				port_b_control: 0,
				port_b_data: 0,
				port_b_pull: 0x00154555, // I do not question the manufacturer, I obey the manufacturer (why is pin 6 different?)
				port_b_select: 0x1,

				port_c_control: 0,
				port_c_data: 0,
				port_c_pull: 0x55555555, // from the manual

				port_d_control: 0,
				port_d_data: 0,
				port_d_pull: 0x55555555, // from the manual

				port_e_control: 0,
				port_e_data: 0,
				port_e_pull: 0x55555555, // from the manual
				port_e_select: 0,

				port_f_control: 0,
				port_f_data: 0,
				port_f_pull: 0x5555, // from the manual

				port_g_control: 0,
				port_g_data: 0,
				port_g_pull: 0x55555555, // from the manual

				port_h_control: 0,
				port_h_data: 0,
				port_h_pull: 0x15555555, // from the manual

				port_k_control: 0xaaaaaaaa,
				port_k_data: 0,
				port_k_pull: 0x55555555, // from the manual

				port_l_control: 0,
				port_l_data: 0,
				port_l_pull: 0x15555555, // from the manual
				port_l_select: 0, // this is from the S3C2451 manual

				port_m_control: 0xa,
				port_m_data: 0,
				port_m_pull: 0, // from the manual
			}
		}
	}
	impl mem::Store for GPIO {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		fn max_addr(&self) -> u32 { BASE_ADDR + SIZE - 1 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> { None }
		fn load_halfword(&mut self, addr: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			match addr {
				PORT_A_CONTROL_ADDR => Some(self.port_a_control),
				PORT_A_DATA_ADDR    => Some(self.port_a_data),

				PORT_B_CONTROL_ADDR => Some(self.port_b_control),
				PORT_B_DATA_ADDR    => Some(self.port_b_data),
				PORT_B_PULL_ADDR    => Some(self.port_b_pull),
				PORT_B_SELECT_ADDR  => Some(self.port_b_select),

				PORT_C_CONTROL_ADDR => Some(self.port_c_control),
				PORT_C_DATA_ADDR    => Some(self.port_c_data),
				PORT_C_PULL_ADDR    => Some(self.port_c_pull),

				PORT_D_CONTROL_ADDR => Some(self.port_d_control),
				PORT_D_DATA_ADDR    => Some(self.port_d_data),
				PORT_D_PULL_ADDR    => Some(self.port_d_pull),

				PORT_E_CONTROL_ADDR => Some(self.port_e_control),
				PORT_E_DATA_ADDR    => Some(self.port_e_data),
				PORT_E_PULL_ADDR    => Some(self.port_e_pull),
				PORT_E_SELECT_ADDR  => Some(self.port_e_select),

				PORT_F_CONTROL_ADDR => Some(self.port_f_control),
				PORT_F_DATA_ADDR    => Some(self.port_f_data),
				PORT_F_PULL_ADDR    => Some(self.port_f_pull),

				PORT_G_CONTROL_ADDR => Some(self.port_g_control),
				PORT_G_DATA_ADDR    => Some(self.port_g_data),
				PORT_G_PULL_ADDR    => Some(self.port_g_pull),

				PORT_H_CONTROL_ADDR => Some(self.port_h_control),
				PORT_H_DATA_ADDR    => Some(self.port_h_data),
				PORT_H_PULL_ADDR    => Some(self.port_h_pull),

				PORT_K_CONTROL_ADDR => Some(self.port_k_control),
				PORT_K_DATA_ADDR    => Some(self.port_k_data),
				PORT_K_PULL_ADDR    => Some(self.port_k_pull),

				PORT_L_CONTROL_ADDR => Some(self.port_l_control),
				PORT_L_DATA_ADDR    => Some(self.port_l_data),
				PORT_L_PULL_ADDR    => Some(self.port_l_pull),
				PORT_L_SELECT_ADDR  => Some(self.port_l_select),

				PORT_M_CONTROL_ADDR => Some(self.port_m_control),
				PORT_M_DATA_ADDR    => Some(self.port_m_data),
				PORT_M_PULL_ADDR    => Some(self.port_m_pull),

				DRIVE_STRENGTH_0_ADDR => Some(self.drive_strength_0),
				DRIVE_STRENGTH_1_ADDR => Some(self.drive_strength_1),
				DRIVE_STRENGTH_2_ADDR => Some(self.drive_strength_2),
				DRIVE_STRENGTH_3_ADDR => Some(self.drive_strength_3),

				STATUS_2_ADDR => Some(SOFTWARE_PLATFORM_ID),
				_ => None,
			}
		}

		fn store_byte(&mut self, addr: u32, data: u8) -> bool { false }
		fn store_halfword(&mut self, addr: u32, data: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				EINT_MASK_ADDR => {
					self.eint_mask = data;
					true
				},
				EINT_PEND_ADDR => {
					for i in 0..32 {
						if data >> i & 0b1 == 0b1 {
							self.eint_pend &= !(1 << i);
						}
					}
					true
				},

				PORT_A_CONTROL_ADDR => { self.port_a_control = data; true },
				PORT_A_DATA_ADDR    => { self.port_a_data    = data; true },

				PORT_B_CONTROL_ADDR => { self.port_b_control = data; true },
				PORT_B_DATA_ADDR    => { self.port_b_data    = data; true },
				PORT_B_PULL_ADDR    => { self.port_b_pull    = data; true },
				PORT_B_SELECT_ADDR  => { self.port_b_select  = data; true },

				PORT_C_CONTROL_ADDR => { self.port_c_control = data; true },
				PORT_C_DATA_ADDR    => { self.port_c_data    = data; true },
				PORT_C_PULL_ADDR    => { self.port_c_pull    = data; true },

				PORT_D_CONTROL_ADDR => { self.port_d_control = data; true },
				PORT_D_DATA_ADDR    => { self.port_d_data    = data; true },
				PORT_D_PULL_ADDR    => { self.port_d_pull    = data; true },

				PORT_E_CONTROL_ADDR => { self.port_e_control = data; true },
				PORT_E_DATA_ADDR    => { self.port_e_data    = data; true },
				PORT_E_PULL_ADDR    => { self.port_e_pull    = data; true },
				PORT_E_SELECT_ADDR  => { self.port_e_select  = data; true },

				PORT_F_CONTROL_ADDR => { self.port_f_control = data; true },
				PORT_F_DATA_ADDR    => { self.port_f_data    = data; true },
				PORT_F_PULL_ADDR    => { self.port_f_pull    = data; true },

				PORT_G_CONTROL_ADDR => { self.port_g_control = data; true },
				PORT_G_DATA_ADDR    => { self.port_g_data    = data; true },
				PORT_G_PULL_ADDR    => { self.port_g_pull    = data; true },

				PORT_H_CONTROL_ADDR => { self.port_h_control = data; true },
				PORT_H_DATA_ADDR    => { self.port_h_data    = data; true },
				PORT_H_PULL_ADDR    => { self.port_h_pull    = data; true },

				PORT_K_CONTROL_ADDR => { self.port_k_control = data; true },
				PORT_K_DATA_ADDR    => { self.port_k_data    = data; true },
				PORT_K_PULL_ADDR    => { self.port_k_pull    = data; true },

				PORT_L_CONTROL_ADDR => { self.port_l_control = data; true },
				PORT_L_DATA_ADDR    => { self.port_l_data    = data; true },
				PORT_L_PULL_ADDR    => { self.port_l_pull    = data; true },
				PORT_L_SELECT_ADDR  => { self.port_l_select  = data; true },

				PORT_M_CONTROL_ADDR => { self.port_m_control = data; true },
				PORT_M_DATA_ADDR    => { self.port_m_data    = data; true },
				PORT_M_PULL_ADDR    => { self.port_m_pull    = data; true },

				DRIVE_STRENGTH_0_ADDR => { self.drive_strength_0 = data; true },
				DRIVE_STRENGTH_1_ADDR => { self.drive_strength_1 = data; true },
				DRIVE_STRENGTH_2_ADDR => { self.drive_strength_2 = data; true },
				DRIVE_STRENGTH_3_ADDR => { self.drive_strength_3 = data; true },

				_ => false,
			}
		}
	}
}

pub mod uart {
	use mem;
	use binutil::{bit32, mask32};

	pub const BASE_ADDR: u32 = 0x5000_0000;
	// the docs list up to base + 0xC02C, I think it's a megabyte but it could be up to 16 megabytes
	pub const SIZE: u32 = 0xC02C + 0x4;

	pub const  LINE_CONTROL_OFFSET: u32 = 0x00;
	pub const       CONTROL_OFFSET: u32 = 0x04;
	pub const  FIFO_CONTROL_OFFSET: u32 = 0x08;
	pub const MODEM_CONTROL_OFFSET: u32 = 0x0c;
	pub const        STATUS_OFFSET: u32 = 0x10;
	pub const  ERROR_STATUS_OFFSET: u32 = 0x14;
	pub const   FIFO_STATUS_OFFSET: u32 = 0x18;
	pub const  MODEM_STATUS_OFFSET: u32 = 0x1c;
	pub const TRANSMIT_DATA_OFFSET: u32 = 0x20;
	pub const  RECEIVE_DATA_OFFSET: u32 = 0x24;
	pub const  BAUD_DIV_INT_OFFSET: u32 = 0x28;
	pub const BAUD_DIV_FRAC_OFFSET: u32 = 0x2c;

	pub enum Mode {
		Disable,
		Normal,
		DMA0,
		DMA1,
	}
	impl Mode {
		pub fn parse(data: u32) -> Self {
			match data {
				0b00 => Mode::Disable,
				0b01 => Mode::Normal,
				0b10 => Mode::DMA0,
				0b11 => Mode::DMA1,
				_ => panic!("bad mode: {:02b}", data),
			}
		}
	}

	pub enum IntType {
		Pulse,
		Level,
	}
	impl IntType {
		pub fn parse(data: u32) -> Self {
			match data {
				0b0 => IntType::Pulse,
				0b1 => IntType::Level,
				_ => panic!("bad interrupt type: {:02b}", data),
			}
		}
	}

	pub struct Modem {
		control: u32,
	}

	pub struct Port {
		line_control: u32,

		clock_selection: u8, // 2 bit, 00 = PCLK, 01 = EXTUARTCLK, 10 = PCLK, 11 = EPLL (divided)
		tx_int_type: IntType,
		rx_int_type: IntType,
		rx_timeout: bool,
		rx_error_int: bool,
		loopback: bool,
		tx_mode: Mode,
		rx_mode: Mode,

		tx_fifo_trigger_level: u8, // 2 bit, 00 = empty, 01 = 16 byte, 10 = 32 byte, 11 = 48 byte
		rx_fifo_trigger_level: u8, // 2 bit, 00 = 1 byte, 01 = 8 byte, 10 = 16 byte, 11 = 32 byte
		fifo: bool,

		modem: Option<Modem>,
		
		// TODO: modem
		// TODO: what is "transmitter empty" (in utrstat)?
		// TODO: data storage

		baud_div_int: u16,
		baud_div_frac: u16,
	}

	pub struct UART {
		ports: [Port; 4],
	}
	impl UART {
		pub fn new() -> Self {
			fn port(modem: bool) -> Port { Port {
				line_control: 0,

				clock_selection: 0,
				tx_int_type: IntType::Pulse,
				rx_int_type: IntType::Pulse,
				rx_timeout: false,
				rx_error_int: false,
				loopback: false,
				tx_mode: Mode::Disable,
				rx_mode: Mode::Disable,

				tx_fifo_trigger_level: 0,
				rx_fifo_trigger_level: 0,
				fifo: false,

				modem: if modem {
					Some(Modem {
						control: 0,
					})
				} else {
					None
				},

				baud_div_int: 1,
				baud_div_frac: 0,
			} }
			Self {
				ports: [port(true), port(true), port(true), port(false)],
			}
		}

		fn decode_addr(&mut self, addr: u32) -> (&mut Port, u32) {
			let addr = addr - BASE_ADDR;
			(&mut self.ports[addr as usize / 0x4000], addr % 0x4000)
		}
	}
	impl mem::Store for UART {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		fn max_addr(&self) -> u32 { BASE_ADDR + SIZE - 1 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> { None }
		fn load_halfword(&mut self, addr: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			let (port, offset) = self.decode_addr(addr);
			match offset {
				STATUS_OFFSET => Some(
					1 << 2 | // transmitter empty
					1 << 1 | // transmit buffer empty
					0 << 0 | // receive buffer data ready
					0
				),
				_ => None,
			}
		}

		fn store_byte(&mut self, addr: u32, data: u8) -> bool {
			let (port, offset) = self.decode_addr(addr);
			match offset {
				TRANSMIT_DATA_OFFSET => true, // TODO
				_ => false,
			}
		}
		fn store_halfword(&mut self, addr: u32, data: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			let (port, offset) = self.decode_addr(addr);
			match offset {
				LINE_CONTROL_OFFSET => {
					port.line_control = data;
					true
				},
				CONTROL_OFFSET => {
					port.clock_selection = (data >> 10 & 0b11) as u8;
					port.tx_int_type = IntType::parse(data >> 9 & 0b1);
					port.rx_int_type = IntType::parse(data >> 8 & 0b1);
					port.rx_timeout = data >> 7 & 0b1 == 0b1;
					port.rx_error_int = data >> 6 & 0b1 == 0b1;
					port.loopback = data >> 5 & 0b1 == 0b1;
					port.tx_mode = Mode::parse(data >> 2 & 0b11);
					port.rx_mode = Mode::parse(data >> 0 & 0b11);
					true
				},
				FIFO_CONTROL_OFFSET => {
					port.tx_fifo_trigger_level = (data >> 6 & 0b11) as u8;
					port.rx_fifo_trigger_level = (data >> 4 & 0b11) as u8;
					if data >> 2 & 0b1 == 0b1 {
						// TODO: clear tx fifo
					}
					if data >> 1 & 0b1 == 0b1 {
						// TODO: clear rx fifo
					}
					port.fifo = data >> 0 & 0b1 == 0b1;
					true
				},
				MODEM_CONTROL_OFFSET => if let Some(ref mut modem) = port.modem {
					modem.control = data;
					true
				} else { false },
				BAUD_DIV_INT_OFFSET => {
					port.baud_div_int = data as u16;
					true
				},
				BAUD_DIV_FRAC_OFFSET => {
					port.baud_div_frac = data as u16;
					true
				},
				_ => false,
			}
		}
	}
}

pub mod interrupts {
	use mem;
	use binutil::{bit32, mask32};

	pub const BASE_ADDR: u32 = 0x4A00_0000;
	pub const SRC_PEND_1_ADDR: u32 = BASE_ADDR + 0x0;
	pub const INT_MODE_1_ADDR: u32 = BASE_ADDR + 0x4;
	pub const INT_MASK_1_ADDR: u32 = BASE_ADDR + 0x8;
	pub const INT_PEND_1_ADDR: u32 = BASE_ADDR + 0x10;
	pub const SUB_SRC_PEND_ADDR: u32 = BASE_ADDR + 0x18;
	pub const INT_SUB_MASK_ADDR: u32 = BASE_ADDR + 0x1c;
	pub const INT_MASK_2_ADDR: u32 = BASE_ADDR + 0x48;
	pub const PRIORITY_MODE_1_ADDR: u32 = BASE_ADDR + 0x30;
	pub const PRIORITY_UPDATE_1_ADDR: u32 = BASE_ADDR + 0x34;
	pub const PRIORITY_MODE_2_ADDR: u32 = BASE_ADDR + 0x70;
	pub const PRIORITY_UPDATE_2_ADDR: u32 = BASE_ADDR + 0x74;
	// the docs list up to base + 0x74, I think it's a megabyte but it could be up to 8 megabytes
	pub const SIZE: u32 = 0x74 + 0x4;

	pub struct Interrupts {
		mode_1: u32,
		pend_1: u32,
		src_pend_1: u32,
		sub_src_pend: u32,

		// TODO: figure out the proper names
		// int mask 1
		mask_adc: bool,
		mask_rtc: bool,
		mask_uart_0: bool,
		mask_iic_0: bool,
		mask_usb_h: bool,
		mask_usb_d: bool,
		mask_nand: bool,
		mask_uart_1: bool,
		mask_spi_0: bool,
		mask_sdi_0: bool,
		mask_sdi_1: bool,
		mask_uart_3: bool,
		mask_dma: bool,
		mask_lcd: bool,
		mask_uart_2: bool,
		mask_timer_4: bool,
		mask_timer_3: bool,
		mask_timer_2: bool,
		mask_timer_1: bool,
		mask_timer_0: bool,
		mask_wdt_ac97: bool,
		mask_tick: bool,
		mask_nbatt_flt: bool,
		mask_eint_8_15: bool,
		mask_eint_4_7: bool,
		mask_eint_3: bool,
		mask_eint_2: bool,
		mask_eint_1: bool,
		mask_eint_0: bool,

		// int mask 2
		mask_i2s0: bool,
		mask_pcm0: bool,
		mask_2d: bool,

		// int sub mask
		mask_ac97: bool,
		mask_wdt: bool,
		mask_uart_3_err: bool,
		mask_uart_3_tx: bool,
		mask_uart_3_rx: bool,
		mask_dma_5: bool,
		mask_dma_4: bool,
		mask_dma_3: bool,
		mask_dma_2: bool,
		mask_dma_1: bool,
		mask_dma_0: bool,
		mask_lcd_4: bool, // i80 I/F
		mask_lcd_3: bool, // LCD Frame
		mask_lcd_2: bool, // LCD FIFO
		mask_adc_sub: bool,
		mask_tc: bool,
		mask_uart_2_err: bool,
		mask_uart_2_tx: bool,
		mask_uart_2_rx: bool,
		mask_uart_1_err: bool,
		mask_uart_1_tx: bool,
		mask_uart_1_rx: bool,
		mask_uart_0_err: bool,
		mask_uart_0_tx: bool,
		mask_uart_0_rx: bool,

		priority_mode_1: u32,
		priority_update_1: u32,
		priority_mode_2: u32,
		priority_update_2: u32,
	}
	impl Interrupts {
		pub fn new() -> Self {
			Self {
				mode_1: 0,
				pend_1: 0,
				src_pend_1: 0,
				sub_src_pend: 0,

				mask_adc: true,
				mask_rtc: true,
				mask_uart_0: true,
				mask_iic_0: true,
				mask_usb_h: true,
				mask_usb_d: true,
				mask_nand: true,
				mask_uart_1: true,
				mask_spi_0: true,
				mask_sdi_0: true,
				mask_sdi_1: true,
				mask_uart_3: true,
				mask_dma: true,
				mask_lcd: true,
				mask_uart_2: true,
				mask_timer_4: true,
				mask_timer_3: true,
				mask_timer_2: true,
				mask_timer_1: true,
				mask_timer_0: true,
				mask_wdt_ac97: true,
				mask_tick: true,
				mask_nbatt_flt: true,
				mask_eint_8_15: true,
				mask_eint_4_7: true,
				mask_eint_3: true,
				mask_eint_2: true,
				mask_eint_1: true,
				mask_eint_0: true,

				mask_i2s0: true,
				mask_pcm0: true,
				mask_2d: true,
				
				mask_ac97: true,
				mask_wdt: true,
				mask_uart_3_err: true,
				mask_uart_3_tx: true,
				mask_uart_3_rx: true,
				mask_dma_5: true,
				mask_dma_4: true,
				mask_dma_3: true,
				mask_dma_2: true,
				mask_dma_1: true,
				mask_dma_0: true,
				mask_lcd_4: true,
				mask_lcd_3: true,
				mask_lcd_2: true,
				mask_adc_sub: true,
				mask_tc: true,
				mask_uart_2_err: true,
				mask_uart_2_tx: true,
				mask_uart_2_rx: true,
				mask_uart_1_err: true,
				mask_uart_1_tx: true,
				mask_uart_1_rx: true,
				mask_uart_0_err: true,
				mask_uart_0_tx: true,
				mask_uart_0_rx: true,

				priority_mode_1: 0,
				priority_update_1: 0x7f,
				priority_mode_2: 0,
				priority_update_2: 0x7f,
			}
		}
	}
	impl mem::Store for Interrupts {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		fn max_addr(&self) -> u32 { BASE_ADDR + SIZE - 1 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> { None }
		fn load_halfword(&mut self, addr: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			match addr {
				INT_PEND_1_ADDR => Some(self.pend_1),
				PRIORITY_MODE_1_ADDR => Some(self.priority_mode_1),
				PRIORITY_UPDATE_1_ADDR => Some(self.priority_update_1),
				PRIORITY_MODE_2_ADDR => Some(self.priority_mode_2),
				PRIORITY_UPDATE_2_ADDR => Some(self.priority_update_2),
				INT_MODE_1_ADDR => Some(self.mode_1),
				INT_MASK_1_ADDR => Some(
					bit32(self.mask_adc)       << 31 |
					bit32(self.mask_rtc)       << 30 |
					1 << 29 |
					bit32(self.mask_uart_0)    << 28 |
					bit32(self.mask_iic_0)     << 27 |
					bit32(self.mask_usb_h)     << 26 |
					bit32(self.mask_usb_d)     << 25 |
					bit32(self.mask_nand)      << 24 |
					bit32(self.mask_uart_1)    << 23 |
					bit32(self.mask_spi_0)     << 22 |
					bit32(self.mask_sdi_0)     << 21 |
					bit32(self.mask_sdi_1)     << 20 |
					1 << 19 |
					bit32(self.mask_uart_3)    << 18 |
					bit32(self.mask_dma)       << 17 |
					bit32(self.mask_lcd)       << 16 |
					bit32(self.mask_uart_2)    << 15 |
					bit32(self.mask_timer_4)   << 14 |
					bit32(self.mask_timer_3)   << 13 |
					bit32(self.mask_timer_2)   << 12 |
					bit32(self.mask_timer_1)   << 11 |
					bit32(self.mask_timer_0)   << 10 |
					bit32(self.mask_wdt_ac97)  <<  9 |
					bit32(self.mask_tick)      <<  8 |
					bit32(self.mask_nbatt_flt) <<  7 |
					1 << 6 |
					bit32(self.mask_eint_8_15) <<  5 |
					bit32(self.mask_eint_4_7)  <<  4 |
					bit32(self.mask_eint_3)    <<  3 |
					bit32(self.mask_eint_2)    <<  2 |
					bit32(self.mask_eint_1)    <<  1 |
					bit32(self.mask_eint_0)    <<  0 |
					0
				),
				INT_SUB_MASK_ADDR => Some(
					1 << 31 |
					1 << 30 |
					1 << 29 |
					bit32(self.mask_ac97)       << 28 |
					bit32(self.mask_wdt)        << 27 |
					bit32(self.mask_uart_3_err) << 26 |
					bit32(self.mask_uart_3_tx)  << 25 |
					bit32(self.mask_uart_3_rx)  << 24 |
					bit32(self.mask_dma_5)      << 23 |
					bit32(self.mask_dma_4)      << 22 |
					bit32(self.mask_dma_3)      << 21 |
					bit32(self.mask_dma_2)      << 20 |
					bit32(self.mask_dma_1)      << 19 |
					bit32(self.mask_dma_0)      << 18 |
					bit32(self.mask_lcd_4)      << 17 |
					bit32(self.mask_lcd_3)      << 16 |
					bit32(self.mask_lcd_2)      << 15 |
					1 << 14 |
					1 << 13 |
					1 << 12 |
					1 << 11 |
					bit32(self.mask_adc_sub)    << 10 |
					bit32(self.mask_tc)         <<  9 |
					bit32(self.mask_uart_2_err) <<  8 |
					bit32(self.mask_uart_2_tx)  <<  7 |
					bit32(self.mask_uart_2_rx)  <<  6 |
					bit32(self.mask_uart_1_err) <<  5 |
					bit32(self.mask_uart_1_tx)  <<  4 |
					bit32(self.mask_uart_1_rx)  <<  3 |
					bit32(self.mask_uart_0_err) <<  2 |
					bit32(self.mask_uart_0_tx)  <<  1 |
					bit32(self.mask_uart_0_rx)  <<  0 |
					0
				),
				_ => None,
			}
		}

		fn store_byte(&mut self, addr: u32, data: u8) -> bool { false }
		fn store_halfword(&mut self, addr: u32, data: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				INT_MASK_1_ADDR => {
					self.mask_adc       = data >> 31 & 0b1 == 0b1;
					self.mask_rtc       = data >> 30 & 0b1 == 0b1;
					self.mask_uart_0    = data >> 28 & 0b1 == 0b1;
					self.mask_iic_0     = data >> 27 & 0b1 == 0b1;
					self.mask_usb_h     = data >> 26 & 0b1 == 0b1;
					self.mask_usb_d     = data >> 25 & 0b1 == 0b1;
					self.mask_nand      = data >> 24 & 0b1 == 0b1;
					self.mask_uart_1    = data >> 23 & 0b1 == 0b1;
					self.mask_spi_0     = data >> 22 & 0b1 == 0b1;
					self.mask_sdi_0     = data >> 21 & 0b1 == 0b1;
					self.mask_sdi_1     = data >> 20 & 0b1 == 0b1;
					self.mask_uart_3    = data >> 18 & 0b1 == 0b1;
					self.mask_dma       = data >> 17 & 0b1 == 0b1;
					self.mask_lcd       = data >> 16 & 0b1 == 0b1;
					self.mask_uart_2    = data >> 15 & 0b1 == 0b1;
					self.mask_timer_4   = data >> 14 & 0b1 == 0b1;
					self.mask_timer_3   = data >> 13 & 0b1 == 0b1;
					self.mask_timer_2   = data >> 12 & 0b1 == 0b1;
					self.mask_timer_1   = data >> 11 & 0b1 == 0b1;
					self.mask_timer_0   = data >> 10 & 0b1 == 0b1;
					self.mask_wdt_ac97  = data >>  9 & 0b1 == 0b1;
					self.mask_tick      = data >>  8 & 0b1 == 0b1;
					self.mask_nbatt_flt = data >>  7 & 0b1 == 0b1;
					self.mask_eint_8_15 = data >>  5 & 0b1 == 0b1;
					self.mask_eint_4_7  = data >>  4 & 0b1 == 0b1;
					self.mask_eint_3    = data >>  3 & 0b1 == 0b1;
					self.mask_eint_2    = data >>  2 & 0b1 == 0b1;
					self.mask_eint_1    = data >>  1 & 0b1 == 0b1;
					self.mask_eint_0    = data >>  0 & 0b1 == 0b1;
					true
				},
				INT_SUB_MASK_ADDR => {
					self.mask_ac97       = data >> 28 & 0b1 == 0b1;
					self.mask_wdt        = data >> 27 & 0b1 == 0b1;
					self.mask_uart_3_err = data >> 26 & 0b1 == 0b1;
					self.mask_uart_3_tx  = data >> 25 & 0b1 == 0b1;
					self.mask_uart_3_rx  = data >> 24 & 0b1 == 0b1;
					self.mask_dma_5      = data >> 23 & 0b1 == 0b1;
					self.mask_dma_4      = data >> 22 & 0b1 == 0b1;
					self.mask_dma_3      = data >> 21 & 0b1 == 0b1;
					self.mask_dma_2      = data >> 20 & 0b1 == 0b1;
					self.mask_dma_1      = data >> 19 & 0b1 == 0b1;
					self.mask_dma_0      = data >> 18 & 0b1 == 0b1;
					self.mask_lcd_4      = data >> 17 & 0b1 == 0b1;
					self.mask_lcd_3      = data >> 16 & 0b1 == 0b1;
					self.mask_lcd_2      = data >> 15 & 0b1 == 0b1;
					self.mask_adc_sub    = data >> 10 & 0b1 == 0b1;
					self.mask_tc         = data >>  9 & 0b1 == 0b1;
					self.mask_uart_2_err = data >>  8 & 0b1 == 0b1;
					self.mask_uart_2_tx  = data >>  7 & 0b1 == 0b1;
					self.mask_uart_2_rx  = data >>  6 & 0b1 == 0b1;
					self.mask_uart_1_err = data >>  5 & 0b1 == 0b1;
					self.mask_uart_1_tx  = data >>  4 & 0b1 == 0b1;
					self.mask_uart_1_rx  = data >>  3 & 0b1 == 0b1;
					self.mask_uart_0_err = data >>  2 & 0b1 == 0b1;
					self.mask_uart_0_tx  = data >>  1 & 0b1 == 0b1;
					self.mask_uart_0_rx  = data >>  0 & 0b1 == 0b1;
					true
				},
				INT_MASK_2_ADDR => {
					self.mask_i2s0 = data >> 6 & 0b1 == 0b1;
					self.mask_pcm0 = data >> 4 & 0b1 == 0b1;
					self.mask_2d   = data >> 0 & 0b1 == 0b1;
					true
				},
				INT_MODE_1_ADDR => {
					self.mode_1 = data;
					true
				},
				SRC_PEND_1_ADDR => {
					for i in 0..32 {
						if data >> i & 0b1 == 0b1 {
							self.src_pend_1 &= !(1 << i);
						}
					}
					true
				},
				INT_PEND_1_ADDR => {
					for i in 0..32 {
						if data >> i & 0b1 == 0b1 {
							self.pend_1 &= !(1 << i);
						}
					}
					true
				},
				SUB_SRC_PEND_ADDR => {
					for i in 0..32 {
						if data >> i & 0b1 == 0b1 {
							self.sub_src_pend &= !(1 << i);
						}
					}
					true
				},
				PRIORITY_MODE_1_ADDR => { self.priority_mode_1 = data; true },
				PRIORITY_UPDATE_1_ADDR => { self.priority_update_1 = data; true },
				PRIORITY_MODE_2_ADDR => { self.priority_mode_2 = data; true },
				PRIORITY_UPDATE_2_ADDR => { self.priority_update_2 = data; true },
				_ => false,
			}
		}
	}
}

pub mod watchdog_timer {
	use mem;
	use binutil::{bit32, mask32};

	pub const BASE_ADDR: u32 = 0x5300_0000;
	pub const CONTROL_ADDR: u32 = BASE_ADDR;
	pub const INITAL_COUNT_ADDR: u32 = BASE_ADDR + 0x4;
	pub const COUNT_ADDR: u32 = BASE_ADDR + 0x8;
	// the docs list up to base + 0x8, I think it's a megabyte but it could be up to 16 megabytes
	pub const SIZE: u32 = 0x8 + 0x4;

	pub struct WatchdogTimer {
		prescaler: u8,
		enable: bool,
		division_factor: u8, // 16 * 2^division_factor
		interrupt: bool,
		reset: bool,
		initial_count: u16,
		count: u16,
	}
	impl WatchdogTimer {
		pub fn new() -> Self {
			Self {
				prescaler: 0x80,
				enable: true,
				division_factor: 0,
				interrupt: false,
				reset: true,
				initial_count: 0x8000,
				count: 0x8000,
			}
		}
	}
	impl mem::Store for WatchdogTimer {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		fn max_addr(&self) -> u32 { BASE_ADDR + SIZE - 1 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> { None }
		fn load_halfword(&mut self, addr: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			match addr {
				CONTROL_ADDR => Some(
					(self.prescaler as u32) << 8 |
					bit32(self.enable) << 5 |
					(self.division_factor as u32) << 3 |
					bit32(self.interrupt) << 2 |
					bit32(self.reset) << 0 |
					0
				),
				_ => None,
			}
		}

		fn store_byte(&mut self, addr: u32, data: u8) -> bool { false }
		fn store_halfword(&mut self, addr: u32, data: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				CONTROL_ADDR => {
					self.prescaler = (data >> 8 & mask32(8)) as u8;
					self.enable = data >> 5 & 0b1 == 0b1;
					self.division_factor = (data >> 3 & 0b11) as u8;
					self.interrupt = data >> 2 & 0b1 == 0b1;
					self.reset = data >> 0 & 0b1 == 0b1;
					true
				},
				_ => false,
			}
		}
	}
}

pub mod pwm_timer {
	use mem;
	use binutil::{bit32, mask32};

	pub const BASE_ADDR: u32 = 0x5100_0000;
	pub const PRESCALER_CONFIG_ADDR: u32 = BASE_ADDR;
	pub const CONTROL_ADDR: u32 = BASE_ADDR + 0x8;
	pub const TIMER_0_COUNT_BUFFER_ADDR: u32 = BASE_ADDR + 0xc;
	pub const TIMER_0_COMPARE_BUFFER_ADDR: u32 = BASE_ADDR + 0x10;
	pub const TIMER_0_COUNT_OBSERV_ADDR: u32 = BASE_ADDR + 0x14;
	// the docs list up to base + 0x40, I think it's a megabyte but it could be up to 16 megabytes
	pub const SIZE: u32 = 0x40 + 0x4;

	pub struct Timer {
		count_buffer: u16,
		compare_buffer: u16,

		count: u16,
		compare: u16,

		auto_reload: bool,
		inverter: bool,
		manual_update: bool,
		run: bool,
	}
	impl Timer {
		fn set_count_buffer(&mut self, val: u16) {
			if self.manual_update {
				panic!("TODO");
			}
			self.count_buffer = val;
		}
		fn set_compare_buffer(&mut self, val: u16) {
			if self.manual_update {
				panic!("TODO");
			}
			self.compare_buffer = val;
		}
		fn set_manual_update(&mut self, val: bool) {
			if val && !self.manual_update {
				self.count = self.count_buffer;
				self.compare = self.compare_buffer;
			}
			self.manual_update = val;
		}
	}

	pub struct PWMTimer {
		dead_zone_length: u8,
		prescaler_0: u8, // timers 0 and 1
		prescaler_1: u8, // timers 2, 3 and 4

		timers: [Timer; 5],
		dead_zone: bool,
	}
	impl PWMTimer {
		pub fn new() -> Self {
			fn timer() -> Timer {
				Timer {
					count_buffer: 0,
					compare_buffer: 0,

					count: 0,
					compare: 0,

					auto_reload: false,
					inverter: false,
					manual_update: false,
					run: false,
				}
			}
			Self {
				dead_zone_length: 0,
				prescaler_0: 0,
				prescaler_1: 0,

				timers: [timer(), timer(), timer(), timer(), timer()],
				dead_zone: false,
			}
		}
	}
	impl mem::Store for PWMTimer {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		fn max_addr(&self) -> u32 { BASE_ADDR + SIZE - 1 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> { None }
		fn load_halfword(&mut self, addr: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			match addr {
				PRESCALER_CONFIG_ADDR => Some(
					(self.dead_zone_length as u32) << 16 |
					(self.prescaler_1 as u32) << 8 |
					(self.prescaler_0 as u32) << 0 |
					0
				),
				CONFIG_ADDR => Some(
					bit32(self.timers[4].auto_reload)   << 22 |
					bit32(self.timers[4].manual_update) << 21 |
					bit32(self.timers[4].run)           << 20 |
					bit32(self.timers[3].auto_reload)   << 19 |
					bit32(self.timers[3].inverter)      << 18 |
					bit32(self.timers[3].manual_update) << 17 |
					bit32(self.timers[3].run)           << 16 |
					bit32(self.timers[2].auto_reload)   << 15 |
					bit32(self.timers[2].inverter)      << 14 |
					bit32(self.timers[2].manual_update) << 13 |
					bit32(self.timers[2].run)           << 12 |
					bit32(self.timers[1].auto_reload)   << 11 |
					bit32(self.timers[1].inverter)      << 10 |
					bit32(self.timers[1].manual_update) <<  9 |
					bit32(self.timers[1].run)           <<  8 |
					bit32(self.dead_zone)               <<  4 |
					bit32(self.timers[0].auto_reload)   <<  3 |
					bit32(self.timers[0].inverter)      <<  2 |
					bit32(self.timers[0].manual_update) <<  1 |
					bit32(self.timers[0].run)           <<  0 |
					0
				),
				_ => None,
			}
		}

		fn store_byte(&mut self, addr: u32, data: u8) -> bool { false }
		fn store_halfword(&mut self, addr: u32, data: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				PRESCALER_CONFIG_ADDR => {
					self.dead_zone_length = (data >> 16 & mask32(8)) as u8;
					self.prescaler_1 = (data >> 8 & mask32(8)) as u8;
					self.prescaler_0 = (data >> 0 & mask32(8)) as u8;
					true
				},
				CONFIG_ADDR => {
					self.timers[4].auto_reload     = data >> 22 & 0b1 == 0b1;
					self.timers[4].set_manual_update(data >> 21 & 0b1 == 0b1);
					self.timers[4].run             = data >> 20 & 0b1 == 0b1;
					self.timers[3].auto_reload     = data >> 19 & 0b1 == 0b1;
					self.timers[3].inverter        = data >> 18 & 0b1 == 0b1;
					self.timers[3].set_manual_update(data >> 17 & 0b1 == 0b1);
					self.timers[3].run             = data >> 16 & 0b1 == 0b1;
					self.timers[2].auto_reload     = data >> 15 & 0b1 == 0b1;
					self.timers[2].inverter        = data >> 14 & 0b1 == 0b1;
					self.timers[2].set_manual_update(data >> 13 & 0b1 == 0b1);
					self.timers[2].run             = data >> 12 & 0b1 == 0b1;
					self.timers[1].auto_reload     = data >> 11 & 0b1 == 0b1;
					self.timers[1].inverter        = data >> 10 & 0b1 == 0b1;
					self.timers[1].set_manual_update(data >>  9 & 0b1 == 0b1);
					self.timers[1].run             = data >>  8 & 0b1 == 0b1;
					self.dead_zone                 = data >>  4 & 0b1 == 0b1;
					self.timers[0].auto_reload     = data >>  3 & 0b1 == 0b1;
					self.timers[0].inverter        = data >>  2 & 0b1 == 0b1;
					self.timers[0].set_manual_update(data >>  1 & 0b1 == 0b1);
					self.timers[0].run             = data >>  0 & 0b1 == 0b1;
					true
				},
				TIMER_0_COUNT_BUFFER_ADDR => { self.timers[0].set_count_buffer(data as u16); true },
				TIMER_0_COMPARE_BUFFER_ADDR => { self.timers[0].set_compare_buffer(data as u16); true },
				_ => false,
			}
		}
	}
}

pub mod sys_con {
	use mem;
	use binutil::{bit32, mask32};

	pub const BASE_ADDR: u32 = 0x4C00_0000;
	pub const MPLL_CONFIG_ADDR: u32 = BASE_ADDR + 0x10;
	pub const CLKSRC_ADDR: u32 = BASE_ADDR + 0x20;
	pub const CLKDIV0_ADDR: u32 = BASE_ADDR + 0x24;
	pub const CLKDIV1_ADDR: u32 = BASE_ADDR + 0x28;
	pub const CLKDIV2_ADDR: u32 = BASE_ADDR + 0x2c;
	pub const HCLK_ENABLE_ADDR: u32 = BASE_ADDR + 0x30;
	pub const PCLK_ENABLE_ADDR: u32 = BASE_ADDR + 0x34;
	pub const SCLK_ENABLE_ADDR: u32 = BASE_ADDR + 0x38;
	pub const RESET_CONTROL_ADDR: u32 = BASE_ADDR + 0x64;
	pub const MPLL_LOCK_TIME_ADDR: u32 = BASE_ADDR;
	pub const SLEEP_0_ADDR: u32 = BASE_ADDR + 0x70;
	pub const SLEEP_1_ADDR: u32 = BASE_ADDR + 0x74;
	pub const SLEEP_2_ADDR: u32 = BASE_ADDR + 0x78;
	pub const SLEEP_3_ADDR: u32 = BASE_ADDR + 0x7c;
	// the docs list up to base + 0x8c, I think it's a megabyte but it could be up to 8 megabytes
	pub const SIZE: u32 = 0x8c + 0x4;

	pub struct SysCon {
		mpll_config: u32,
		mpll_lock_time: u16,
		clksrc: u32,
		clkdiv0: u32,
		clkdiv1: u32,
		clkdiv2: u32,
		hclk_enable: u32,
		pclk_enable: u32,
		sclk_enable: u32,
		reset_control: u32,
		sleep: [u32; 4],
	}
	impl SysCon {
		pub fn new() -> Self {
			Self {
				mpll_config: 0x185_40c0,
				mpll_lock_time: 0xffff,
				clksrc: 0,
				clkdiv0: 0xc,
				clkdiv1: 0,
				clkdiv2: 0,
				hclk_enable: 0xffff_ffff,
				pclk_enable: 0xffff_ffff,
				sclk_enable: 0xffff_dfff,
				reset_control: 0x6_0101,
				sleep: [0; 4],
			}
		}
	}
	impl mem::Store for SysCon {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		fn max_addr(&self) -> u32 { BASE_ADDR + SIZE - 1 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> { None }
		fn load_halfword(&mut self, addr: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			match addr {
				MPLL_CONFIG_ADDR    => Some(self.mpll_config),
				MPLL_LOCK_TIME_ADDR => Some(self.mpll_lock_time as u32),
				CLKSRC_ADDR         => Some(self.clksrc),
				CLKDIV0_ADDR        => Some(self.clkdiv0),
				CLKDIV1_ADDR        => Some(self.clkdiv1),
				CLKDIV2_ADDR        => Some(self.clkdiv2),
				HCLK_ENABLE_ADDR    => Some(self.hclk_enable),
				PCLK_ENABLE_ADDR    => Some(self.pclk_enable),
				SCLK_ENABLE_ADDR    => Some(self.sclk_enable),
				RESET_CONTROL_ADDR  => Some(self.reset_control),
				SLEEP_0_ADDR        => Some(self.sleep[0]),
				SLEEP_1_ADDR        => Some(self.sleep[1]),
				SLEEP_2_ADDR        => Some(self.sleep[2]),
				SLEEP_3_ADDR        => Some(self.sleep[3]),
				_                   => None,
			}
		}

		fn store_byte(&mut self, addr: u32, data: u8) -> bool { false }
		fn store_halfword(&mut self, addr: u32, data: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				MPLL_CONFIG_ADDR    => { self.mpll_config    = data; true },
				MPLL_LOCK_TIME_ADDR => { self.mpll_lock_time = data as u16; true },
				CLKSRC_ADDR         => { self.clksrc         = data; true },
				CLKDIV0_ADDR        => { self.clkdiv0        = data; true },
				CLKDIV1_ADDR        => { self.clkdiv1        = data; true },
				CLKDIV2_ADDR        => { self.clkdiv2        = data; true },
				HCLK_ENABLE_ADDR    => { self.hclk_enable    = data; true },
				PCLK_ENABLE_ADDR    => { self.pclk_enable    = data; true },
				SCLK_ENABLE_ADDR    => { self.sclk_enable    = data; true },
				RESET_CONTROL_ADDR  => { self.reset_control  = data; true },
				SLEEP_0_ADDR        => { self.sleep[0]       = data; true },
				SLEEP_1_ADDR        => { self.sleep[1]       = data; true },
				SLEEP_2_ADDR        => { self.sleep[2]       = data; true },
				SLEEP_3_ADDR        => { self.sleep[3]       = data; true },
				_                   => false,
			}
		}
	}
}

pub mod dram_con {
	use mem;
	use binutil::{bit32, mask32};

	pub const BASE_ADDR: u32 = 0x4800_0000;
	pub const CONFIG_ADDR: u32 = BASE_ADDR;
	pub const CONTROL_ADDR: u32 = BASE_ADDR + 0x4;
	pub const TIMING_CONTROL_ADDR: u32 = BASE_ADDR + 0x8;
	pub const EMRS_ADDR: u32 = BASE_ADDR + 0xc;
	pub const REFRESH_CONTROL_ADDR: u32 = BASE_ADDR + 0x10;
	// the docs list up to base + 0x14, I think it's a megabyte but it could be up to 8 megabytes
	pub const SIZE: u32 = 0x14 + 0x4;

	pub struct DRAMCon {
		config: u32,
		control: u32,
		timing_control: u32,
		emrs: u32,
		refresh_control: u32,
	}
	impl DRAMCon {
		pub fn new() -> Self {
			Self {
				config: 0xc,
				control: 0x4400_0040,
				timing_control: 0x99_003f,
				emrs: 0x8000_0003,
				refresh_control: 0x20,
			}
		}
	}
	impl mem::Store for DRAMCon {
		fn min_addr(&self) -> u32 { BASE_ADDR }
		fn max_addr(&self) -> u32 { BASE_ADDR + SIZE - 1 }

		fn load_byte(&mut self, addr: u32) -> Option<u8> { None }
		fn load_halfword(&mut self, addr: u32) -> Option<u16> { None }
		fn load_word(&mut self, addr: u32) -> Option<u32> {
			match addr {
				CONFIG_ADDR => Some(self.config),
				CONTROL_ADDR => Some(self.control),
				TIMING_CONTROL_ADDR => Some(self.timing_control),
				EMRS_ADDR => Some(self.emrs),
				REFRESH_CONTROL_ADDR => Some(self.refresh_control),
				_ => None,
			}
		}

		fn store_byte(&mut self, addr: u32, data: u8) -> bool { false }
		fn store_halfword(&mut self, addr: u32, data: u16) -> bool { false }
		fn store_word(&mut self, addr: u32, data: u32) -> bool {
			match addr {
				CONFIG_ADDR => { self.config = data; true },
				CONTROL_ADDR => { self.control = data; true },
				TIMING_CONTROL_ADDR => { self.timing_control = data; true },
				EMRS_ADDR => { self.emrs = data; true },
				REFRESH_CONTROL_ADDR => { self.refresh_control = data; true },
				_ => false,
			}
		}
	}
}
