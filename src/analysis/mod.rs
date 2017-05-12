pub mod parser;
pub mod print;
pub mod structures;

use mem;
use instr::*;
use disasm;
use pp;

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
