use instr::*;

impl Condition {
	pub fn pp(self) -> &'static str {
		match self {
			Condition::Equal       => "eq",
			Condition::NotEqual    => "ne",
			Condition::UnsignedGTE => "uge",
			Condition::UnsignedLT  => "ul",
			Condition::Negative    => "neg",
			Condition::NonNegative => "pos",
			Condition::Overflow    => "so",
			Condition::NoOverflow  => "sno",
			Condition::UnsignedGT  => "ug",
			Condition::UnsignedLTE => "ule",
			Condition::SignedGTE   => "sge",
			Condition::SignedLT    => "sl",
			Condition::SignedGT    => "sg",
			Condition::SignedLTE   => "sle",
			Condition::Always      => "",
			Condition::Custom      => "C",
		}
	}
}

impl Register {
	pub fn pp(self) -> String {
		format!("r{}", self.0)
	}
}

impl StatusRegister {
	pub fn pp(self) -> &'static str {
		match self {
			StatusRegister::Current => "cpsr",
			StatusRegister::Saved => "spsr",
		}
	}
}

impl ShiftOpShift {
	pub fn pp(&self) -> String {
		match self {
			&ShiftOpShift::Immediate(i) => format!("0x{:x}", i),
			&ShiftOpShift::Register(r) => r.pp(),
		}
	}
}

impl ShiftOp {
	pub fn pp(&self) -> String {
		match self {
			&ShiftOp::LSL(ref shift) => format!("LSL {}", shift.pp()),
			&ShiftOp::LSR(ref shift) => format!("LSR {}", shift.pp()),
			&ShiftOp::ASR(ref shift) => format!("ASR {}", shift.pp()),
			&ShiftOp::ROR(ref shift) => format!("ROR {}", shift.pp()),
			&ShiftOp::RRX => format!("RRX"),
		}
	}
}

impl ShiftOperand {
	pub fn pp(&self) -> String {
		match self {
			&ShiftOperand::Immediate { imm, rotate } => format!("0x{:x}", (imm as u32).rotate_right(rotate as u32 * 2)),
			&ShiftOperand::Register { reg, ref shift } => format!("{} {}", reg.pp(), shift.pp()),
		}
	}
}

impl StoreStatusOp {
	pub fn pp(&self) -> String {
		match self {
			&StoreStatusOp::Register(r) => r.pp(),
			&StoreStatusOp::Immediate { imm, rotate } => format!("0x{:x}", (imm as u32).rotate_right(rotate as u32 * 2)),
		}
	}
}

impl<N, A, SA, SS, LN, LA> MulDest<N, A, SA, SS, LN, LA> {
	pub fn pp(&self) -> String {
		match self {
			&MulDest::Normal(_, d) => d.pp(),
			&MulDest::Accumulate(_, d) => format!("{}+", d.pp()),
			&MulDest::SeparateAccumulate(_, d, a) => format!("{}, +{}", d.pp(), a.pp()),
			&MulDest::SeparateAccumulateSubtract(_, d, a) => format!("{}, +{}", d.pp(), a.pp()),
			&MulDest::LongNormal { lo, hi, .. } => format!("{}, {}", lo.pp(), hi.pp()),
			&MulDest::LongAccumulate { lo, hi, .. } => format!("{}+, {}+", lo.pp(), hi.pp()),
		}
	}
}

impl Instr {
	pub fn pp(&self, pc: u32) -> String {
		match self {
			&Instr::Data { cond, ref op, update_status, dest, op1, ref op2 } => match op {
				&DataOp::Move => format!("mov{}{} {}, {}"
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op2.pp()
				),
				&DataOp::Negate => format!("neg{}{} {}, {}"
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op2.pp()
				),
				&DataOp::Compare => format!("cmp{} {}, {}", cond.pp(), op1.pp(), op2.pp()),
				&DataOp::CompareNegative => format!("cmn{} {}, {}", cond.pp(), op1.pp(), op2.pp()),
				&DataOp::Test => format!("tst{} {}, {}", cond.pp(), op1.pp(), op2.pp()),
				&DataOp::TestEq => format!("teq{} {}, {}", cond.pp(), op1.pp(), op2.pp()),
				&DataOp::Add { carry } => format!("ad{}{}{} {}, {}, {}"
					, if carry { "c" } else { "d" }
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op1.pp()
					, op2.pp()
				),
				&DataOp::Subtract { reverse, carry } => format!("{}{}{} {}, {}, {}"
					, match (reverse, carry) {
						(false, false) => "sub",
						(false, true) => "sbc",
						(true, false) => "rsb",
						(true, true) => "rsc",
					}
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op1.pp()
					, op2.pp()
				),
				&DataOp::And => format!("and{}{} {}, {}, {}"
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op1.pp()
					, op2.pp()
				),
				&DataOp::BitClear => format!("bic{}{} {}, {}, {}"
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op1.pp()
					, op2.pp()
				),
				&DataOp::XOr => format!("xor{}{} {}, {}, {}"
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op1.pp()
					, op2.pp()
				),
				&DataOp::Or => format!("orr{}{} {}, {}, {}"
					, cond.pp()
					, if update_status { "s" } else { "" }
					, dest.pp()
					, op1.pp()
					, op2.pp()
				),
			},
			&Instr::LoadStore { cond, load, data, addr, ref mode, upwards, byte, ref priv_walk } => {
				let offset = format!("{}{}"
					, if upwards { "+" } else { "-" }
					, match mode {
						&AddrMode::Register { reg, ref shift } => format!("{} {}", reg.pp(), shift.pp()),
						&AddrMode::Immediate(i) => format!("0x{:x}", i),
					}
				);
				format!("{}{}{}{} {}, [{}{}"
					, if load { "ldr" } else { "str" }
					, cond.pp()
					, if byte { "b" } else { "" }
					, if let &PrivWalk::Post { unprivileged: true } = priv_walk {
						"t"
					} else {
						""
					}
					, data.pp()
					, addr.pp()
					, match priv_walk {
						&PrivWalk::Post { .. } => format!("!], {}", offset),
						&PrivWalk::Pre { walk } => format!("{}, {}]"
							, if walk { "!" } else { "" }
							, offset
						),
					}
				)
			},
			&Instr::Jump { cond, link, addr } => format!("b{}{} 0x{:x}"
				, if link { "l" } else { "" }
				, cond.pp()
				, pc.wrapping_add(8).wrapping_add((addr as i32).wrapping_shl(8).wrapping_shr(6) as u32)
			),
			&Instr::CallThumb { addr } => format!("blx 0x{:x}"
				, pc.wrapping_add(8).wrapping_add((addr as i32).wrapping_shl(8).wrapping_shr(7) as u32)
			),
			&Instr::ThumbyJump { cond, link, addr } => format!("b{}x{} {}"
				, if link { "l" } else { "" }
				, cond.pp()
				, addr.pp()
			),
			&Instr::LoadStoreMultiple { cond, load, base_excluded, upwards, exc_thing, walk, registers, base } => {
				format!("{}{} {}{}{}{}, {}{{{}}}"
					, if load { "ldm" } else { "stm" }
					, cond.pp()
					, if upwards { "+" } else { "-" }
					, if base_excluded {
						if upwards { "+" } else { "-" }
					} else { "" }
					, base.pp()
					, if walk { "!" } else { "" }
					, if exc_thing && registers >> 15 & 0b1 == 0b0 {
						"unbanked"
					} else { "" }
					, {
						let mut str = String::new();
						let mut first = true;
						if exc_thing && registers >> 15 & 0b1 == 0b1 {
							str = format!("{}psr", str);
							first = false;
						};
						for i in 0 ... 15 {
							if registers >> i & 0b1 == 0b1 {
								str = format!("{}{}r{}", str, if !first { ", " } else { "" }, i);
								first = false;
							}
						};
						str
					}
				)
			},
			&Instr::Mul { cond, ref op, op1, op2 } => {
				format!("{}{}{} {}, {}, {}"
					, match op {
						&MulOp::Lo32 { dest: MulDest::Normal(_, _), .. } => "mul".to_owned(),
						&MulOp::Lo32 { dest: MulDest::SeparateAccumulate(_, _, _), .. } => "mla".to_owned(),
						&MulOp::Hi32 { ref dest, round } => format!("{}{}"
							, match dest {
								&MulDest::Normal(_, _) => "smmul",
								&MulDest::SeparateAccumulate(_, _, _) => "smmla",
								&MulDest::SeparateAccumulateSubtract(_, _, _) => "smmls",
								_ => unreachable!(),
							}
							, if round { "r" } else { "" }
						),
						&MulOp::Long { signed, ref dest, .. } => format!("{}{}"
							, if signed { "s" } else { "u" }
							, match dest {
								&MulDest::LongNormal { .. } => "mull",
								&MulDest::LongAccumulate { .. } => "mlal",
								_ => unreachable!(),
							}
						),
						&MulOp::UMAAL { .. } => "umaal".to_owned(),
						&MulOp::Halfword { ref dest, op1_hi, op2_hi } => format!("{}{}{}"
							, match dest {
								&MulDest::Normal(_, _) => "smul",
								&MulDest::SeparateAccumulate(_, _, _) => "smla",
								&MulDest::LongAccumulate { .. } => "smlal",
								_ => unreachable!(),
							}
							, if op1_hi { "h" } else { "l" }
							, if op2_hi { "h" } else { "l" }
						),
						&MulOp::WordHalfword { ref dest, op2_hi } => format!("{}{}"
							, match dest {
								&MulDest::Normal(_, _) => "smulw",
								&MulDest::SeparateAccumulate(_, _, _) => "smlaw",
								_ => unreachable!(),
							}
							, if op2_hi { "h" } else { "l" }
						),
						&MulOp::DualHalfword { ref dest, subtract, flip } => {
							let subtract = if subtract { "s" } else { "a" };
							format!("{}{}"
								, match dest {
									&MulDest::Normal(_, _) => format!("smu{}d", subtract),
									&MulDest::SeparateAccumulate(_, _, _) => format!("sml{}d", subtract),
									&MulDest::LongAccumulate { .. } => format!("sml{}ld", subtract),
									_ => unreachable!(),
								}
								, if flip { "x" } else { "" }
							)
						},
						_ => unreachable!(),
					}
					, cond.pp()
					, match op {
						&MulOp::Lo32 { update_status: true, .. } => "s",
						&MulOp::Long { update_status: true, .. } => "s",
						_ => "",
					}
					, match op {
						&MulOp::Lo32 { ref dest, .. } => dest.pp(),
						&MulOp::Hi32 { ref dest, .. } => dest.pp(),
						&MulOp::Long { ref dest, .. } => dest.pp(),
						&MulOp::UMAAL { lo, hi } => format!("{}+, +{}+", lo.pp(), hi.pp()),
						&MulOp::Halfword { ref dest, .. } => dest.pp(),
						&MulOp::WordHalfword { ref dest, .. } => dest.pp(),
						&MulOp::DualHalfword { ref dest, .. } => dest.pp(),
					}
					, op1.pp()
					, op2.pp()
				)
			},
			&Instr::Coprocessor { cond, op, coproc, opcode1, opcode2, reg, creg1, creg2 } => format!("{}{} {}, {}, {}, c{}, c{}, {}"
				, match op {
					None => "cdp",
					Some(CoprocessorTransfer::To) => "mcr",
					Some(CoprocessorTransfer::From) => "mrc",
				}
				, if let Condition::Custom = cond {
					"2"
				} else {
					cond.pp()
				}
				, coproc
				, opcode1
				, if let None = op {
					format!("c{}", reg.0)
				} else {
					reg.pp()
				}
				, creg1.0
				, creg2.0
				, opcode2
			),
			&Instr::CoprocessorMem { cond, op, coproc, upwards, ref mode, n, creg, addr } => format!("{}{}{} {}, c{}, [{}{}"
				, match op {
					CoprocessorTransfer::To => "ldc",
					CoprocessorTransfer::From => "stc",
				}
				, if let Condition::Custom = cond {
					"2"
				} else {
					cond.pp()
				}
				, if n { "l" } else { "" }
				, coproc
				, creg.0
				, addr.pp()
				, match mode {
					&CoprocessorAddrMode::Pre { walk, offset } => format!("{}, {}0x{:x}"
						, if walk { "!" } else { "" }
						, if upwards { "+" } else { "-" }
						, offset
					),
					&CoprocessorAddrMode::Post { offset } => format!("], {}0x{:x}"
						, if upwards { "+" } else { "-" }
						, offset
					),
					&CoprocessorAddrMode::Unindexed { arg } => format!("!], {}", arg),
				}
			),
			&Instr::MiscLoadStore { cond, load, data, addr, mode, upwards, walk } => {
				let offset = format!("{}{}"
					, if upwards { "+" } else { "-" }
					, match mode {
						MiscAddrMode::Immediate(i) => format!("0x{:x}", i),
						MiscAddrMode::Register(r) => r.pp(),
					}
				);
				format!("{}{}{} {}, [{}{}"
					, match load {
						MiscLoadStore::Halfword(None) => "str",
						MiscLoadStore::Halfword(Some(_)) => "ldr",
						MiscLoadStore::LoadSignedByte => "ldr",
						MiscLoadStore::Doubleword { load: false } => "str",
						MiscLoadStore::Doubleword { load: true } => "ldr",
					}
					, cond.pp()
					, match load {
						MiscLoadStore::Halfword(None) => "h",
						MiscLoadStore::Halfword(Some(false)) => "h",
						MiscLoadStore::Halfword(Some(true)) => "sh",
						MiscLoadStore::LoadSignedByte => "sb",
						MiscLoadStore::Doubleword { .. } => "d",
					}
					, if let MiscLoadStore::Doubleword { .. } = load {
						format!("{}, {}", data.pp(), Register(data.0 + 1).pp())
					} else {
						data.pp()
					}
					, addr.pp()
					, match walk {
						MiscWalk::Post => format!("!], {}", offset),
						MiscWalk::Pre { walk } => format!("{}, {}]", if walk { "!" } else { "" }, offset),
					}
				)
			},
			&Instr::LoadStatus { cond, dst, src } => format!("mrs{} {}, {}"
				, cond.pp()
				, dst.pp()
				, src.pp()
			),
			&Instr::StoreStatus { cond, dst, ref src, mask_c, mask_x, mask_s, mask_f } => format!("msr{} {}_{}{}{}{}, {}"
				, cond.pp()
				, dst.pp()
				, if mask_c { "c" } else { "" }
				, if mask_x { "x" } else { "" }
				, if mask_s { "s" } else { "" }
				, if mask_f { "f" } else { "" }
				, src.pp()
			),
			&Instr::SoftwareInterrupt { cond, imm } => format!("swi{} 0x{:x}", cond.pp(), imm),
			_ => format!("TODO: {:?}", self),
		}
	}
}
