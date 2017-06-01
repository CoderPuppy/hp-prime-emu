use instr::*;
use binutil::mask16;

pub fn disasm_cond(asm: u16) -> Condition {
	match asm {
		0b0000 => Condition::Equal,
		0b0001 => Condition::NotEqual,
		0b0010 => Condition::UnsignedGTE,
		0b0011 => Condition::UnsignedLT,
		0b0100 => Condition::Negative,
		0b0101 => Condition::NonNegative,
		0b0110 => Condition::Overflow,
		0b0111 => Condition::NoOverflow,
		0b1000 => Condition::UnsignedGT,
		0b1001 => Condition::UnsignedLTE,
		0b1010 => Condition::SignedGTE,
		0b1011 => Condition::SignedLT,
		0b1100 => Condition::SignedGT,
		0b1101 => Condition::SignedLTE,
		0b1110 => Condition::Always,
		0b1111 => Condition::Custom,
		_ => panic!("condition isn't a 4 bit number: {:b}", asm),
	}
}

pub fn disasm_reg(asm: u16) -> Register {
	assert!(asm <= 0b111);
	Register(asm as u8)
}

pub fn disasm_instr<F: FnMut() -> u16>(mut read: F) -> Option<Instr> {
	let asm = read();
	if asm & 0b111_00_00000_000_000 == 0b000_00_00000_000_000
		&& asm & 0b000_11_00000_000_000 != 0b000_11_00000_000_000 {
		let op = ShiftOpShift::Immediate((asm >> 6 & mask16(5)) as u8);
		Some(Instr::Data {
			cond: Condition::Always,
			op: DataOp::Move,
			update_status: true,
			dest: disasm_reg(asm >> 0 & 0b111),
			op1: Register(0),
			op2: ShiftOperand::Register {
				reg: disasm_reg(asm >> 3 & 0b111),
				shift: match asm >> 11 & 0b11 {
					0b00 => ShiftOp::LSL(op),
					0b01 => ShiftOp::LSR(op),
					0b10 => ShiftOp::ASR(op),
					_ => unreachable!(),
				},
			},
		})
	} else if asm & 0b111_11_0_0_000_000_000 == 0b000_11_0_0_000_000_000 {
		Some(Instr::Data {
			cond: Condition::Always,
			op: if asm >> 9 & 0b1 == 0b1 {
				DataOp::Subtract { reverse: false, carry: false }
			} else {
				DataOp::Add { carry: false }
			},
			update_status: true,
			dest: disasm_reg(asm & 0b111),
			op1: disasm_reg(asm >> 3 & 0b111),
			op2: if asm >> 10 & 0b1 == 0b1 {
				ShiftOperand::Immediate {
					imm: (asm >> 6 & 0b111) as u8,
					rotate: 0,
				}
			} else {
				ShiftOperand::Register {
					reg: disasm_reg(asm >> 6 & 0b111),
					shift: ShiftOp::LSL(ShiftOpShift::Immediate(0)),
				}
			},
		})
	} else if asm & 0b111_00_000_00000000 == 0b001_00_000_00000000 {
		Some(Instr::Data {
			cond: Condition::Always,
			op: match asm >> 11 & 0b11 {
				0b00 => DataOp::Move,
				0b01 => DataOp::Compare,
				0b10 => DataOp::Add { carry: false },
				0b11 => DataOp::Subtract { reverse: false, carry: false },
				_ => unreachable!(),
			},
			update_status: true,
			dest: if asm >> 11 & 0b11 == 0b01 { // compare
				Register(0)
			} else {
				disasm_reg(asm >> 8 & 0b111)
			},
			op1: if asm >> 11 & 0b11 == 0b00 { // move
				Register(0)
			} else {
				disasm_reg(asm >> 8 & 0b111)
			},
			op2: ShiftOperand::Immediate {
				imm: (asm & mask16(8)) as u8,
				rotate: 0,
			},
		})
	} else if asm & 0b111111_0000_000_000 == 0b010000_0000_000_000 {
		let opcode = asm >> 6 & 0b1111;
		let r1 = disasm_reg(asm & 0b111);
		let r2 = disasm_reg(asm >> 3 & 0b111);
		Some(match opcode {
			0b0000 | 0b0001 | 0b0101 | 0b0110 | 0b1100 | 0b1110 => Instr::Data {
				// and, eor, adc, sbc, orr, bic
				cond: Condition::Always,
				op: match opcode {
					0b0000 => DataOp::And,
					0b0001 => DataOp::XOr,
					0b0101 => DataOp::Add { carry: true },
					0b0110 => DataOp::Subtract { reverse: false, carry: true },
					0b1100 => DataOp::Or,
					0b1110 => DataOp::BitClear,
					_ => unreachable!(),
				},
				update_status: true,
				dest: r1,
				op1: r1,
				op2: ShiftOperand::Register {
					reg: r2,
					shift: ShiftOp::LSL(ShiftOpShift::Immediate(0)),
				},
			},
			0b0010 | 0b0011 | 0b0100 | 0b0111 => Instr::Data {
				// lsl, lsr, asr, ror
				cond: Condition::Always,
				op: DataOp::Move,
				update_status: true,
				dest: r1,
				op1: Register(0),
				op2: ShiftOperand::Register {
					reg: r1,
					shift: match opcode {
						0b0010 => ShiftOp::LSL(ShiftOpShift::Register(r2)),
						0b0011 => ShiftOp::LSR(ShiftOpShift::Register(r2)),
						0b0100 => ShiftOp::ASR(ShiftOpShift::Register(r2)),
						0b0111 => ShiftOp::ROR(ShiftOpShift::Register(r2)),
						_ => unreachable!(),
					},
				},
			},
			0b1000 | 0b1010 | 0b1011 => Instr::Data {
				// cmp, cmn, tst
				cond: Condition::Always,
				op: match opcode {
					0b1000 => DataOp::Test,
					0b1010 => DataOp::Compare,
					0b1011 => DataOp::CompareNegative,
					_ => unreachable!(),
				},
				update_status: true,
				dest: Register(0),
				op1: r1,
				op2: ShiftOperand::Register {
					reg: r2,
					shift: ShiftOp::LSL(ShiftOpShift::Immediate(0)),
				},
			},
			0b1001 => Instr::Data {
				// neg
				cond: Condition::Always,
				op: DataOp::Subtract { reverse: true, carry: true },
				update_status: true,
				dest: r1,
				op1: r2,
				op2: ShiftOperand::Immediate {
					imm: 0,
					rotate: 0,
				},
			},
			0b1111 => Instr::Data {
				// mvn
				cond: Condition::Always,
				op: DataOp::Negate,
				update_status: true,
				dest: r1,
				op1: Register(0),
				op2: ShiftOperand::Register {
					reg: r2,
					shift: ShiftOp::LSL(ShiftOpShift::Immediate(0)),
				},
			},
			0b1101 => Instr::Mul {
				cond: Condition::Always,
				op: MulOp::Lo32 {
					dest: MulDest::Normal(is(), r1),
					update_status: true,
				},
				op1: r2,
				op2: r1,
			},
			_ => unreachable!(),
		})
	} else if asm & 0b111111_0000_000_000 == 0b010001_00_0_0_000_000 {
		let opcode = asm >> 8 & 0b11;
		let r1 = disasm_reg(asm & 0b111);
		let r2 = disasm_reg(asm >> 3 & 0b111);
		let r1 = if asm >> 7 & 0b1 == 0b1 {
			Register(r1.0 + 8)
		} else {
			r1
		};
		let r2 = if asm >> 6 & 0b1 == 0b1 {
			Register(r2.0 + 8)
		} else {
			r2
		};
		match opcode {
			0b10 => Some(Instr::Data {
				// mov
				cond: Condition::Always,
				op: DataOp::Move,
				update_status: false,
				dest: r1,
				op1: Register(0),
				op2: ShiftOperand::Register {
					reg: r2,
					shift: ShiftOp::LSL(ShiftOpShift::Immediate(0)),
				},
			}),
			0b00 => Some(Instr::Data {
				// add
				cond: Condition::Always,
				op: DataOp::Add { carry: false },
				update_status: false,
				dest: r1,
				op1: r1,
				op2: ShiftOperand::Register {
					reg: r2,
					shift: ShiftOp::LSL(ShiftOpShift::Immediate(0)),
				},
			}),
			0b01 => Some(Instr::Data {
				// cmp
				cond: Condition::Always,
				op: DataOp::Compare,
				update_status: true,
				dest: Register(0),
				op1: r1,
				op2: ShiftOperand::Register {
					reg: r2,
					shift: ShiftOp::LSL(ShiftOpShift::Immediate(0)),
				},
			}),
			0b11 => Some(Instr::ThumbyJump {
				cond: Condition::Always,
				link: asm >> 7 & 0b1 == 0b1,
				addr: r2,
			}),
			_ => unreachable!(),
		}
	} else if asm & 0b111_0_0_00000_000_000 == 0b011_0_0_00000_000_000 {
		Some(Instr::LoadStore {
			cond: Condition::Always,
			load: asm >> 11 & 0b1 == 0b1,
			data: disasm_reg(asm & 0b111),
			addr: disasm_reg(asm >> 3 & 0b111),
			mode: AddrMode::Immediate((asm >> 6 & mask16(5)) as u16),
			upwards: true,
			byte: asm >> 12 & 0b1 == 0b1,
			priv_walk: PrivWalk::Pre { walk: false },
		})
	} else if asm & 0b1111_0_000_00000000 == 0b1100_0_000_00000000 {
		Some(Instr::LoadStoreMultiple {
			cond: Condition::Always,
			load: asm >> 11 & 0b1 == 0b1,
			base_excluded: false,
			upwards: true,
			exc_thing: false,
			walk: true,
			registers: (asm & mask16(8)) as u16,
			base: disasm_reg(asm >> 8 & 0b111),
		})
	} else if asm & 0b1111_0000_00000000 == 0b1101_0000_00000000
		&& asm >> 9 & mask16(3) != 0b111 {
		Some(Instr::ThumbJump {
			cond: disasm_cond(asm >> 8 & 0b1111),
			addr: (asm & mask16(8)) as i8 as i16,
		})
	} else if asm & 0b111_11_00000000000 == 0b111_00_00000000000 {
		Some(Instr::ThumbJump {
			cond: Condition::Always,
			addr: ((asm & mask16(11)) as i16).wrapping_shl(5).wrapping_shr(5),
		})
	} else if asm & 0b111_11_00000000000 == 0b111_10_00000000000 {
		Some(Instr::ThumbJumpPrefix {
			offset: ((asm & mask16(11)) as i16).wrapping_shl(5).wrapping_shr(5),
		})
	} else if asm & 0b111_01_00000000000 == 0b111_01_00000000000 {
		Some(Instr::ThumbCall {
			offset: asm & mask16(11),
			exchange: asm >> 12 & 0b1 == 0b0,
		})
	} else {
		None
	}
}
