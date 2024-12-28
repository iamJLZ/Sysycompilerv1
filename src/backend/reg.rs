use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Reg {
	Zero,
	Ra,
	Sp,
	Gp,
	Tp,
	T0,
	T1,
	T2,
	S0,
	S1,
	A0,
	A1,
	A2,
	A3,
	A4,
	A5,
	A6,
	A7,
	S2,
	S3,
	S4,
	S5,
	S6,
	S7,
	S8,
	S9,
	S10,
	S11,
	T3,
	T4,
	T5,
	T6,
}

impl Display for Reg{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Reg::Zero=>write!(f,"zero"),
			Reg::Ra => write!(f,"ra"),
			Reg::Sp => write!(f,"sp"),
			Reg::Gp => write!(f,"gp"),
			Reg::Tp => write!(f,"tp"),
			Reg::T0 => write!(f,"t0"),
			Reg::T1 => write!(f,"t1"),
			Reg::T2 => write!(f,"t2"),
			Reg::S0 => write!(f,"s0"),
			Reg::S1 => write!(f,"s1"),
			Reg::A0 => write!(f,"a0"),
			Reg::A1 => write!(f,"a1"),
			Reg::A2 => write!(f,"a2"),
			Reg::A3 => write!(f,"a3"),
			Reg::A4 => write!(f,"a4"),
			Reg::A5 => write!(f,"a5"),
			Reg::A6 => write!(f,"a6"),
			Reg::A7 => write!(f,"a7"),
			Reg::S2 => write!(f,"s2"),
			Reg::S3 => write!(f,"s3"),
			Reg::S4 => write!(f,"s4"),
			Reg::S5 => write!(f,"s5"),
			Reg::S6 => write!(f,"s6"),
			Reg::S7 => write!(f,"s7"),
			Reg::S8 => write!(f,"s8"),
			Reg::S9 => write!(f,"s9"),
			Reg::S10=> write!(f,"s10"),
			Reg::S11=> write!(f,"s11"),
			Reg::T3 => write!(f,"t3"),
			Reg::T4 => write!(f,"t4"),
			Reg::T5 => write!(f,"t5"),
			Reg::T6 => write!(f,"t6"),
		}
	}
}