use mem;
use instr::*;
use disasm;
use pp;
use cell::WCell;
use std::sync::{Arc, Weak};
use std::marker::PhantomData;
use std::collections::BTreeMap;
use store::Id;

pub struct LinearCode {
	pub start: u32,
	pub end: u32,
	pub info: Vec<(u32, CodeInfo)>,
}
pub enum JumpDir {
	To,
	From,
}
pub enum CodeInfo {
	Jump {
		dir: JumpDir,
		addr: u32,
		link: bool,
	},
	JumpedTo,
	Access {
		store: bool,
		addr: u32,
		len: u32,
	},
	DisableObvious,
}

pub enum RangeInfo {
	AccessedBy {
		addr: u32,
		store: bool,
	},
}

pub enum Widget {
	// TODO
}

pub enum Note {
	Note(String),
	Widget(Id<Widget>),
}

pub enum LogicalChild {
	Logical(Id<LogicalBlock>),
	Physical(Id<PhysicalBlock>),
	Note(Note),
}

// #[derive(Debug)]
pub struct LogicalBlock {
	pub name: String,
	pub children: Vec<LogicalChild>,
}

// #[derive(Debug)]
pub struct PhysicalBlock {
	pub start: u32,
	pub end: u32,
	pub name: Option<String>,
	pub parent: Option<Id<PhysicalBlock>>,
	pub notes: Vec<(u32, Note)>,
	pub sub_blocks: Vec<(u32, Id<PhysicalBlock>)>,
	pub logical_parent: Option<Id<LogicalBlock>>,
	pub widget_uses: BTreeMap<Id<Widget>, ()>,
}

pub fn analyse_forward<S: mem::Store>(src: S, start_addr: u32) {
	for addr in (start_addr..).step_by(4) {
		let instr_raw = src.load_word(addr).unwrap();
		let instr = Instr::disasm(instr_raw);
		println!("{:08x} {}", addr, instr.pp(addr));
		match instr {
			Instr::LoadStore { addr: Register(15), mode: AddrMode::Immediate(offset), priv_walk: PrivWalk::Pre { .. }, upwards, byte, .. } => {
				let start = addr.wrapping_add(8);
				let start = if upwards {
					start + offset as u32
				} else {
					start - offset as u32
				};
				let end = if byte {
					start
				} else {
					start + 3
				};
				println!("- {:08x} load from {:08x}-{:08x}", addr, start, end);
			},
			_ => {},
		}
		match instr {
			Instr::Data { cond: Condition::Always, op, update_status, dest: Register(15), op1, op2 } => match op {
				DataOp::Compare | DataOp::CompareNegative | DataOp::Test | DataOp::TestEq => {},
				_ => break,
			},
			Instr::LoadStore { cond: Condition::Always, load: true, data: Register(15), .. } => break,
			Instr::LoadStore { cond: Condition::Always, addr: Register(15), priv_walk, .. } => match priv_walk {
				PrivWalk::Post { .. } => break,
				PrivWalk::Pre { walk: true } => break,
				_ => {},
			},
			Instr::Jump { cond: Condition::Always, link, addr: to } => {
				let to = addr.wrapping_add(8).wrapping_add((to as u32) << 2);
				println!("- {:08x} jump to {:08x}", addr, to);
				if !link && (to >= addr || to < start_addr) {
					break
				}
			},
			Instr::LoadStoreMultiple { cond: Condition::Always, load: true, registers, .. } if registers >> 15 & 0b1 == 0b1 => break,
			_ => {},
		}
	}
}
