use instr::*;
use binutil::mask;

impl Condition {
	pub fn disasm(asm: u32) -> Self {
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
}

impl Register {
	pub fn disasm(asm: u32) -> Self {
		assert!(asm <= 0b1111);
		Register(asm as u8)
	}
}

impl ShiftOperand {
	pub fn disasm(asm: u32) -> Self {
		if asm >> 25 & 0b1 == 0b1 {
			ShiftOperand::Immediate {
				imm: (asm >> 0) as u8,
				rotate: (asm >> 8 & 0b1111) as u8,
			}
		} else {
			let operand = if asm >> 4 & 0b1 == 0b0 {
				ShiftOpShift::Immediate((asm >> 7 & 0b11111) as u8)
			} else {
				ShiftOpShift::Register(Register::disasm(asm >> 8 & 0b1111))
			};
			ShiftOperand::Register {
				reg: Register::disasm(asm & 0b1111),
				shift: match asm >> 5 & 0b11 {
					0b00 => ShiftOp::LSL(operand),
					0b01 => ShiftOp::LSR(operand),
					0b10 => ShiftOp::ASR(operand),
					0b11 => if (asm & 0b111110010000) == 0 {
						ShiftOp::RRX
					} else {
						ShiftOp::ROR(operand)
					},
					_ => unreachable!(),
				},
			}
		}
	}
}

impl AddrMode {
	pub fn disasm(asm: u32) -> Self {
		if asm >> 25 & 0b1 == 0b1 {
			let operand = if asm >> 4 & 0b1 == 0b0 {
				ShiftOpShift::Immediate((asm >> 7 & 0b11111) as u8)
			} else {
				ShiftOpShift::Register(Register::disasm(asm >> 8 & 0b1111))
			};
			AddrMode::Register {
				reg: Register::disasm(asm & 0b1111),
				shift: match asm >> 5 & 0b11 {
					0b00 => ShiftOp::LSL(operand),
					0b01 => ShiftOp::LSR(operand),
					0b10 => ShiftOp::ASR(operand),
					0b11 => if (asm & 0b111110010000) == 0 {
						ShiftOp::RRX
					} else {
						ShiftOp::ROR(operand)
					},
					_ => unreachable!(),
				},
			}
		} else {
			AddrMode::Immediate((asm & mask(12)) as u16)
		}
	}
}

impl Instr {
	pub fn disasm(asm: u32) -> Self {
		let cond = Condition::disasm(asm >> 28 & 0b1111);
		if        asm & 0b0000_110_0000_0_0000_0000_000000000000 == 0b0000_010_0000_0_0000_0000_000000000000 {
			Instr::LoadStore {
				cond: cond,
				load: asm >> 20 & 0b1 == 0b1,
				addr: Register::disasm(asm >> 16 & 0b1111),
				data: Register::disasm(asm >> 12 & 0b1111),
				mode: AddrMode::disasm(asm),
				upwards: asm >> 23 & 0b1 == 0b1,
				byte: asm >> 22 & 0b1 == 0b1,
				priv_walk: match asm >> 24 & 0b1 {
					0b0 => PrivWalk::Post { unprivileged: asm >> 21 & 0b1 == 0b1, },
					0b1 => PrivWalk::Pre { walk: asm >> 21 & 0b1 == 0b1, },
					_ => unreachable!(),
				},
			}
		} else if asm & 0b0000_110_0000_0_0000_0000_000000000000 == 0b0000_000_0000_0_0000_0000_000000000000
			// prevent overlap with halfword multiplications
			&& (!asm >> 25 & asm >> 4 & asm >> 7 & 0b1) == 0b0
			// prevent overlap with the control and dsp extension space (objdump erronously displays these as cmp, cmn, tst and teq)
			&& (asm >> 24 & 0b1 == 0b0 || asm >> 23 & 0b1 == 0b1 || asm >> 20 & 0b1 == 0b1) {
			Instr::Data {
				cond: cond,
				op: match asm >> 21 & 0b1111 {
					0b0000 => DataOp::And,
					0b0001 => DataOp::XOr,
					0b0010 | 0b0011 | 0b0110 | 0b0111 => DataOp::Subtract { carry: asm >> 23 & 0b1 == 0b1, reverse: asm >> 21 & 0b1 == 0b1, },
					0b0100 | 0b0101 => DataOp::Add { carry: asm >> 21 & 0b1 == 0b1, },
					0b1000 => DataOp::Test,
					0b1001 => DataOp::TestEq,
					0b1010 => DataOp::Compare,
					0b1011 => DataOp::CompareNegative,
					0b1100 => DataOp::Or,
					0b1101 => DataOp::Move,
					0b1110 => DataOp::BitClear,
					0b1111 => DataOp::Negate,
					_ => unreachable!(),
				},
				update_status: asm >> 20 & 0b1 == 0b1,
				dest: Register::disasm(asm >> 12 & 0b1111),
				op1: Register::disasm(asm >> 16 & 0b1111),
				op2: ShiftOperand::disasm(asm),
			}
		} else if asm & 0b0000_111_0000_0_0000_0000_000000000000 == 0b0000_101_0000_0_0000_0000_000000000000 {
			Instr::Jump {
				cond: cond,
				link: asm >> 24 & 0b1 == 0b1,
				addr: ((asm & mask(24)) as i32).wrapping_shl(8).wrapping_shr(8),
			}
		} else if asm & 0b0000_111_1000_0_0000_0000_0000_1111_0000 == 0b0000_000_0000_0_0000_0000_0000_1001_0000 {
			Instr::Mul {
				cond: cond,
				op: match asm >> 20 & 0b1111 {
					_ if asm >> 22 & 0b11 == 0 => MulOp::Lo32 {
						dest: match asm >> 21 & 0b1 {
							0b0 => MulDest::Normal(is(), Register::disasm(asm >> 16 & 0b1111)),
							0b1 => MulDest::SeparateAccumulate(is(), Register::disasm(asm >> 16 & 0b1111), Register::disasm(asm >> 12 & 0b1111)),
							_ => unreachable!(),
						},
						update_status: asm >> 20 & 0b1 == 0b1,
					},
					_ if asm >> 23 & 0b1 == 0b1 => MulOp::Long {
						dest: if asm >> 21 & 0b1 == 0b1 {
							MulDest::LongAccumulate {
								is: is(),
								lo: Register::disasm(asm >> 12 & 0b1111),
								hi: Register::disasm(asm >> 16 & 0b1111),
							}
						} else {
							MulDest::LongNormal {
								is: is(),
								lo: Register::disasm(asm >> 12 & 0b1111),
								hi: Register::disasm(asm >> 16 & 0b1111),
							}
						},
						signed: asm >> 22 & 0b1 == 0b1,
						update_status: asm >> 20 & 0b1 == 0b1,
					},
					0b0100 => MulOp::UMAAL {
						lo: Register::disasm(asm >> 12 & 0b1111),
						hi: Register::disasm(asm >> 16 & 0b1111),
					},
					_ => return Instr::Unimplemented(asm),
				},
				op1: Register::disasm(asm >> 0 & 0b1111),
				op2: Register::disasm(asm >> 8 & 0b1111),
			}
		} else if asm & 0b0000_111_1100_1_0000_0000_0000_1001_0000 == 0b0000_000_1000_0_0000_0000_0000_1000_0000
			&& asm >> 21 & 0b11 != 0b01 {
			Instr::Mul {
				cond: cond,
				op: MulOp::Halfword {
					dest: match asm >> 21 & 0b11 {
						0b11 => MulDest::Normal(is(), Register::disasm(asm >> 16 & 0b1111)),
						0b00 => MulDest::SeparateAccumulate(is(), Register::disasm(asm >> 16 & 0b1111), Register::disasm(asm >> 12 & 0b1111)),
						0b10 => MulDest::LongAccumulate {
							is: is(),
							lo: Register::disasm(asm >> 12 & 0b1111),
							hi: Register::disasm(asm >> 16 & 0b1111),
						},
						_ => unreachable!(),
					},
					op1_hi: asm >> 5 & 0b1 == 0b1,
					op2_hi: asm >> 6 & 0b1 == 0b1,
				},
				op1: Register::disasm(asm >> 0 & 0b1111),
				op2: Register::disasm(asm >> 8 & 0b1111),
			}
		} else if asm & 0b0000_111_0000_0_0000_0000000000000000 == 0b0000_100_0000_0_0000_0000000000000000 {
			Instr::LoadStoreMultiple {
				cond: cond,
				load: asm >> 20 & 0b1 == 0b1,
				base_excluded: asm >> 24 & 0b1 == 0b1,
				upwards: asm >> 23 & 0b1 == 0b1,
				exc_thing: asm >> 22 & 0b1 == 0b1,
				walk: asm >> 21 & 0b1 == 0b1,
				registers: (asm & mask(16)) as u16,
				base: Register::disasm(asm >> 16 & 0b1111),
			}
		} else if asm & 0b0000_111_1111_1_0000_0000_0000_1101_0000 == 0b0000_000_1001_0_0000_0000_0000_0001_0000 {
			Instr::Thumby {
				cond: cond,
				link: asm >> 5 & 0b1 == 0b1,
				addr: Register::disasm(asm & 0b1111),
			}
		} else if asm & 0b0000_111_1000_0_0000_0000_0000_0000_0000 == 0b0000_111_0000_0_0000_0000_0000_0000_0000 {
			Instr::Coprocessor {
				cond: cond,
				op: match asm >> 4 & 0b1 {
					0b0 => None,
					0b1 => Some(match asm >> 20 & 0b1 {
						0b0 => CoprocessorTransfer::To,
						0b1 => CoprocessorTransfer::From,
						_ => unreachable!(),
					}),
					_ => unreachable!(),
				},
				coproc: (asm >> 8 & 0b1111) as u8,
				opcode1: match asm >> 4 & 0b1 {
					0b0 => asm >> 20 & 0b1111,
					0b1 => asm >> 21 & 0b111,
					_ => unreachable!(),
				} as u8,
				opcode2: (asm >> 5 & 0b111) as u8,
				reg: Register::disasm(asm >> 12 & 0b1111),
				creg1: Register::disasm(asm >> 16 & 0b1111),
				creg2: Register::disasm(asm >>  0 & 0b1111),
			}
		} else if asm & 0b0000_111_0000_0_0000_0000_0000_0000_0000 == 0b0000_110_0000_0_0000_0000_0000_0000_0000 {
			let data = (asm >> 0 & mask(8)) as u8;
			let walk = asm >> 21 & 0b1 == 0b1;
			Instr::CoprocessorMem {
				cond: cond,
				op: match asm >> 20 & 0b1 {
					0b0 => CoprocessorTransfer::To,
					0b1 => CoprocessorTransfer::From,
					_ => unreachable!(),
				},
				coproc: (asm >> 8 & 0b1111) as u8,
				upwards: asm >> 23 & 0b1 == 0b1,
				mode: if asm >> 24 & 0b1 == 0b1 {
					CoprocessorAddrMode::Pre { walk: walk, offset: data }
				} else {
					if walk {
						CoprocessorAddrMode::Post { offset: data }
					} else {
						CoprocessorAddrMode::Unindexed { arg: data }
					}
				},
				n: asm >> 22 & 0b1 == 0b1,
				creg: Register::disasm(asm >> 12 & 0b1111),
				addr: Register::disasm(asm >> 16 & 0b1111),
			}
		} else if asm & 0b0000_111_0000_0_0000_0000_0000_1001_0000 == 0b0000_000_0000_0_0000_0000_0000_1001_0000
			&& asm >> 5 & 0b11 != 0b00 {
			Instr::MiscLoadStore {
				cond: cond,
				load: match asm >> 18 & 0b100 | asm >> 5 & 0b11 {
					0b001 => MiscLoadStore::Halfword(None),
					0b010 => MiscLoadStore::Doubleword { load: true, },
					0b011 => MiscLoadStore::Doubleword { load: false, },
					0b101 => MiscLoadStore::Halfword(Some(false)),
					0b110 => MiscLoadStore::LoadSignedByte,
					0b111 => MiscLoadStore::Halfword(Some(true)),
					_ => unreachable!(),
				},
				data: Register::disasm(asm >> 12 & 0b1111),
				addr: Register::disasm(asm >> 16 & 0b1111),
				mode: match asm >> 22 & 0b1 {
					0b0 => MiscAddrMode::Register(Register::disasm(asm >> 0 & 0b1111)),
					0b1 => MiscAddrMode::Immediate((asm >> 4 & 0b11110000 | asm >> 0 & 0b1111) as u8),
					_ => unreachable!(),
				},
				upwards: asm >> 23 & 0b1 == 0b1,
				walk: match asm >> 24 & 0b1 {
					0b0 => MiscWalk::Post,
					0b1 => MiscWalk::Pre { walk: asm >> 21 & 0b1 == 0b1, },
					_ => unreachable!(),
				},
			}
		} else if asm & 0b0000_111_1111_1_0000_0000_0000_1111_0000 == 0b0000_000_1011_0_0000_0000_0000_0001_0000 {
			Instr::CountLeadingZeros {
				cond: cond,
				dest: Register::disasm(asm >> 12 & 0b1111),
				src: Register::disasm(asm >> 0 & 0b1111),
			}
		} else if asm & 0b0000_111_1101_1_0000_0000_0000_0000_0000 == 0b0000_000_1000_0_0000_0000_0000_0000_0000 {
			Instr::LoadStatus {
				cond: cond,
				dst: Register::disasm(asm >> 12 & 0b1111),
				src: match asm >> 22 & 0b1 {
					0b0 => StatusRegister::Current,
					0b1 => StatusRegister::Saved,
					_ => unreachable!(),
				},
			}
		} else if asm & 0b0000_111_1101_1_0000_0000_0000_0000_0000 == 0b0000_001_1001_0_0000_0000_0000_0000_0000 {
			Instr::StoreStatus {
				cond: cond,
				dst: match asm >> 22 & 0b1 {
					0b0 => StatusRegister::Current,
					0b1 => StatusRegister::Saved,
					_ => unreachable!(),
				},
				src: StoreStatusOp::Immediate {
					imm: (asm >> 0 & mask(8)) as u8,
					rotate: (asm >> 8 & 0b1111) as u8,
				},
				mask_c: asm >> 16 & 0b1 == 0b1,
				mask_x: asm >> 17 & 0b1 == 0b1,
				mask_s: asm >> 18 & 0b1 == 0b1,
				mask_f: asm >> 19 & 0b1 == 0b1,
			}
		} else if asm & 0b0000_111_1101_1_0000_0000_0000_1111_0000 == 0b0000_000_1001_0_0000_0000_0000_0000_0000 {
			Instr::StoreStatus {
				cond: cond,
				dst: match asm >> 22 & 0b1 {
					0b0 => StatusRegister::Current,
					0b1 => StatusRegister::Saved,
					_ => unreachable!(),
				},
				src: StoreStatusOp::Register(Register::disasm(asm >> 0 & 0b1111)),
				mask_c: asm >> 16 & 0b1 == 0b1,
				mask_x: asm >> 17 & 0b1 == 0b1,
				mask_s: asm >> 18 & 0b1 == 0b1,
				mask_f: asm >> 19 & 0b1 == 0b1,
			}
		} else if asm & 0b0000_111_1000_0_0000_0000_0000_0000_0000 == 0b0000_111_1000_0_0000_0000_0000_0000_0000 {
			Instr::SoftwareInterrupt {
				cond: cond,
				imm: asm & mask(24),
			}
		} else {
			Instr::Unimplemented(asm)
		}
	}
}
