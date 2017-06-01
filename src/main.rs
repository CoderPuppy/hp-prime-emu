#![feature(step_by, inclusive_range_syntax, heap_api, alloc)]
extern crate libc;
extern crate byteorder;
extern crate cpuprofiler;
extern crate alloc;

use std::fs::File;

mod instr;
mod disasm;
mod thumb_disasm;
mod mem;
mod emu;
mod binutil;
mod pp;
mod gui;
// mod analysis;
// mod cell;
// mod store;

use mem::Store;
use emu::*;
use instr::*;
// use analysis::print::PrettyPrint;
use std::io::Write;
use cpuprofiler::PROFILER;

// const OS_FILE: &'static str = "../ripem/bin/ripem/PRIME_OS.ROM";
// const OS_FILE: &'static str = "../HP_Prime_Calculator_Firmware_20160829/PRIME_OS.ROM";
const OS_FILE: &'static str = "../prime_08-09-SDK0.30/BXCBOOT0.BIN";

fn print_psr(psr: &emu::StatusRegister) -> String {
	format!("{}{}{}{}{}{}{}{} {}"
		, if psr.negative { "N" } else { " " }
		, if psr.zero { "Z" } else { " " }
		, if psr.carry { "C" } else { " " }
		, if psr.overflow { "V" } else { " " }
		, if psr.dsp_overflow { "Q" } else { " " }
		, match psr.instruction_set {
			InstructionSet::ARM => "A",
			InstructionSet::Thumb => "T",
			InstructionSet::Jazelle => "J",
		}
		, if psr.disable_irq { "I" } else { " " }
		, if psr.disable_fiq { "F" } else { " " }
		, match psr.mode {
			Mode::User       => "usr",
			Mode::FIQ        => "fiq",
			Mode::IRQ        => "irq",
			Mode::Supervisor => "svc",
			Mode::Abort      => "abt",
			Mode::Undefined  => "und",
			Mode::System     => "sys",
		}
	)
}
fn print_state(emu: &Emulator) {
	println!("CPSR: {}", print_psr(&emu.regs.cpsr));
	// if let Some(spsr) = emu.regs.get_spsr() {
	// 	println!("SPSR: {}", print_psr(spsr));
	// };
	print!("regs: ");
	for i in 0...15 {
		let val = emu.regs.get(Register(i));
		if i != 0 {
			print!(", ")
		}
		print!("{:x}", val)
	}
	println!();
}

struct StdOut(pub std::io::Stdout);
impl std::fmt::Write for StdOut {
	fn write_str(&mut self, s: &str) -> std::fmt::Result {
		self.0.write(s.as_bytes()).unwrap();
		Ok(())
	}
}

fn main() {
	// PROFILER.lock().unwrap().start("./prof.profile").expect("couldn't start");

	let nand_store = mem::MmapStore::from_file(&File::open(OS_FILE).unwrap(), 0, None).unwrap();
	let mut emu = Emulator::new(Config {
		nand: nand::Config {
			store: Box::new(nand_store),
		},
	});

	// let os_store = mem::MmapStore::from_file(&File::open(OS_FILE).unwrap(), 0, None).unwrap();
	// let os_store = mem::OffsetStore(os_store.load_word(3 * 4).unwrap(), os_store);
	let mut os_store = mem::MmapStore::from_file(&File::open(OS_FILE).unwrap(), 0, Some(0x2000)).unwrap();
	// let start_addr = os_store.load_word(os_store.min_addr()).unwrap();
	// println!("os {:016x}-{:016x}", os_store.min_addr(), os_store.max_addr());
	for addr in (os_store.min_addr()...os_store.max_addr()).step_by(4) {
		emu.store_word_phys(addr, os_store.load_word(addr).unwrap()).unwrap();
	}

	// analysis::analyse_forward(os_store, 0x3007f238);

	// let map_store = mem::MmapStore::from_file(&File::open("test.map").unwrap(), 0, None).unwrap();
	// let mut parser = analysis::parser::Parser::new(map_store, 0);
	// let mut block = analysis::structures::Block {
	// 	start: 0,
	// 	end: u32::max_value(),
	// 	name: None,
	// 	is_code: false,
	// 	notes: Vec::new(),
	// 	sub_blocks: Vec::new(),
	// };
	// loop {
	// 	if let Err(err) = parser.parse_block(&mut block) {
	// 		println!("Error at {}: {}", parser.idx, err);
	// 		break
	// 	}
	// }
	// block.pretty_print(&mut StdOut(std::io::stdout()), "").unwrap();
	// println!();
  //
	// for blk in block.lookup(0x300000d0, 0x300000df, Vec::new()) {
	// 	println!("{:08x}-{:08x}", blk.start, blk.end);
	// }

	// PROFILER.lock().unwrap().stop().expect("couldn't stop");

	// emu.regs.main[15] = emu.load_word(os_store.min_addr()).unwrap();
	print_state(&emu);
	loop {
		emu.tick();
		print_state(&emu);
	}
	
	// for i in (os_store.load_word(os_store.min_addr()).unwrap()...os_store.max_addr()).step_by(4) {
	// 	let instr_raw = os_store.load_word(i).unwrap_or_else(|| panic!("memory: {:x}", i));
	// 	println!("{:x} {:08x} {}", i, instr_raw, Instr::disasm(instr_raw).pp(i));
	// }
	
	// gui::run();
}
