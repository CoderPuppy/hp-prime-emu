use std::marker::PhantomData;

#[derive(Debug)]
pub struct Is<L, R>(PhantomData<L>, PhantomData<R>);
pub fn is<T>() -> Is<T, T> { Is(PhantomData, PhantomData) }

#[derive(Debug)]
pub struct True;

#[derive(Debug)]
pub struct False;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Condition {
	Equal,
	NotEqual,
	UnsignedGTE,
	UnsignedLT,
	Negative,
	NonNegative,
	Overflow,
	NoOverflow,
	UnsignedGT,
	UnsignedLTE,
	SignedGTE,
	SignedLT,
	SignedGT,
	SignedLTE,
	Always,
	Custom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Register(pub u8); // 4 bit

#[derive(Debug)]
pub enum ShiftOpShift {
	Immediate(u8 /* 5 bit */),
	Register(Register),
}
#[derive(Debug)]
pub enum ShiftOp {
	LSL(ShiftOpShift),
	LSR(ShiftOpShift),
	ASR(ShiftOpShift),
	ROR(ShiftOpShift), // no Immediate(0)
	RRX,
}
#[derive(Debug)]
pub enum ShiftOperand {
	Immediate {
		imm: u8,
		rotate: u8, // 4 bit
	},
	Register {
		reg: Register,
		shift: ShiftOp,
	},
}

#[derive(Debug)]
pub enum AddrMode {
	Register {
		reg: Register, 
		shift: ShiftOp,
	},
	Immediate(u16), // 12 bit
}

#[derive(Debug)]
pub enum DataOp {
	Move, Negate,
	Compare, CompareNegative,
	Test, TestEq,
	Add { carry: bool, },
	Subtract {
		reverse: bool,
		carry: bool,
	}, And, BitClear, XOr, Or
}

#[derive(Debug)]
pub enum MulDest<N, A, SA, SAS, LN, LA> {
	Normal(Is<N, True>, Register),
	Accumulate(Is<A, True>, Register),
	SeparateAccumulate(Is<SA, True>, Register, Register),
	SeparateAccumulateSubtract(Is<SAS, True>, Register, Register),
	LongNormal {
		is: Is<LN, True>,
		lo: Register,
		hi: Register,
	},
	LongAccumulate {
		is: Is<LA, True>,
		lo: Register,
		hi: Register,
	},
}

#[derive(Debug)]
pub enum MulOp {
	Lo32 {
		dest: MulDest<True, False, True, False, False, False>,
		update_status: bool,
	},
	Hi32 { // signed
		dest: MulDest<True, False, True, True, False, False>,
		round: bool,
	},
	Long {
		dest: MulDest<False, False, False, False, True, True>,
		signed: bool,
		update_status: bool,
	},
	UMAAL { lo: Register, hi: Register }, // unsigned
	Halfword { // signed
		dest: MulDest<True, False, True, False, False, True>, // long is accumulate
		op1_hi: bool,
		op2_hi: bool,
	},
	WordHalfword { // signed
		dest: MulDest<True, False, True, False, False, False>,
		op2_hi: bool,
	},
	DualHalfword {
		dest: MulDest<True, False, True, True, False, True>, // normal and long are accumulate
		subtract: bool,
		flip: bool,
	},
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoprocessorTransfer {
	To, From
}

#[derive(Debug)]
pub enum PrivWalk {
	Post { unprivileged: bool },
	Pre { walk: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiscLoadStore {
	Halfword(Option<bool>), // None is store, Some(<signed>) is load <signed>
	LoadSignedByte,
	Doubleword { load: bool, },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiscWalk {
	Post,
	Pre { walk: bool, },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiscAddrMode {
	Immediate(u8),
	Register(Register),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatusRegister {
	Current,
	Saved,
}

#[derive(Debug)]
pub enum StoreStatusOp {
	Register(Register),
	Immediate {
		imm: u8,
		rotate: u8, // 4 bit
	},
}

#[derive(Debug)]
pub enum CoprocessorAddrMode {
	Pre { walk: bool, offset: u8 },
	Post { offset: u8 },
	Unindexed { arg: u8 },
}

#[derive(Debug)]
pub enum Instr {
	Data {
		cond: Condition,
		op: DataOp,
		update_status: bool,
		dest: Register,
		op1: Register,
		op2: ShiftOperand,
	},
	LoadStore {
		cond: Condition,
		load: bool,
		data: Register,
		addr: Register,
		mode: AddrMode,
		upwards: bool,
		byte: bool,
		priv_walk: PrivWalk,
	},
	Jump {
		cond: Condition,
		link: bool,
		addr: i32, // 24 bit
	},
	CallThumb {
		addr: i32, // 25 bit
	},
	Thumby {
		cond: Condition,
		link: bool,
		addr: Register,
	},
	LoadStoreMultiple {
		cond: Condition,
		load: bool,
		base_excluded: bool,
		upwards: bool,
		exc_thing: bool,
		walk: bool,
		registers: u16,
		base: Register,
	},
	Mul {
		cond: Condition,
		op: MulOp,
		op1: Register,
		op2: Register,
	},
	Coprocessor {
		cond: Condition, // Custom means Always with a different opcode mapping
		op: Option<CoprocessorTransfer>, // None means no transfer (`CDP`)
		coproc: u8, // 4 bit
		opcode1: u8, // 3 bit for transfer, 4 bit for no transfer
		opcode2: u8, // 3 bit
		reg: Register, // R15 is condition flags when op == Some(From)
		creg1: Register,
		creg2: Register,
	},
	CoprocessorDual {
		cond: Condition, // Custom means Always with a different opcode mapping
		op: CoprocessorTransfer,
		coproc: u8, // 4 bit
		opcode: u8, // 4 bit
		reg1: Register,
		reg2: Register,
		creg: Register,
	},
	CoprocessorMem {
		cond: Condition, // Custom means Always with a different opcode mapping
		op: CoprocessorTransfer,
		coproc: u8, // 4 bit
		upwards: bool,
		mode: CoprocessorAddrMode,
		n: bool,
		creg: Register,
		addr: Register,
	},
	MiscLoadStore {
		cond: Condition,
		load: MiscLoadStore,
		data: Register,
		addr: Register,
		mode: MiscAddrMode,
		upwards: bool,
		walk: MiscWalk,
	},
	CountLeadingZeros {
		cond: Condition,
		dest: Register,
		src: Register,
	},
	LoadStatus {
		cond: Condition,
		dst: Register,
		src: StatusRegister,
	},
	StoreStatus {
		cond: Condition,
		dst: StatusRegister,
		src: StoreStatusOp,
		mask_c: bool,
		mask_x: bool,
		mask_s: bool,
		mask_f: bool,
	},
	SoftwareInterrupt {
		cond: Condition,
		imm: u32, // 24 bit
	},
	Unimplemented(u32),
}
impl Instr {
	pub fn cond(&self) -> Condition {
		match self {
			&Instr::Data { ref cond, .. } => cond.to_owned(),
			&Instr::LoadStore { ref cond, .. } => cond.to_owned(),
			&Instr::Jump { ref cond, .. } => cond.to_owned(),
			&Instr::CallThumb { .. } => Condition::Always,
			&Instr::Thumby { ref cond, .. } => cond.to_owned(),
			&Instr::LoadStoreMultiple { ref cond, .. } => cond.to_owned(),
			&Instr::Mul { ref cond, .. } => cond.to_owned(),
			&Instr::Coprocessor { ref cond, .. } => cond.to_owned(),
			&Instr::CoprocessorDual { ref cond, .. } => cond.to_owned(),
			&Instr::CoprocessorMem { ref cond, .. } => cond.to_owned(),
			&Instr::MiscLoadStore { ref cond, .. } => cond.to_owned(),
			&Instr::CountLeadingZeros { ref cond, .. } => cond.to_owned(),
			&Instr::LoadStatus { ref cond, .. } => cond.to_owned(),
			&Instr::StoreStatus { ref cond, .. } => cond.to_owned(),
			&Instr::SoftwareInterrupt { ref cond, .. } => cond.to_owned(),
			&Instr::Unimplemented(_) => panic!("unimplemented"),
		}
	}
}
