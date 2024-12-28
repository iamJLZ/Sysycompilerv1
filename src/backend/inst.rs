use std::fmt::Display;
use super::reg::Reg;

#[derive(Debug)]
pub enum Ins {
	Beqz(Reg, String),
	Bnez(Reg, String),
	Beq(Reg, Reg, String),
	Bne(Reg, Reg, String),
	Blt(Reg, Reg, String),
	Bge(Reg, Reg, String),
	J(String),
	Call(String),
	Ret,
	Lw(Reg, Reg, i32),
	Sw(Reg, Reg, i32),
	Add(Reg, Reg, Reg),
	Addi(Reg, Reg, i32),
	Sub(Reg, Reg, Reg),
	Slt(Reg, Reg, Reg),
	Sgt(Reg, Reg, Reg),
	Seqz(Reg, Reg),
	Snez(Reg, Reg),
	Xor(Reg, Reg, Reg),
	Xori(Reg, Reg, i32),
	Or(Reg, Reg, Reg),
	Ori(Reg, Reg, i32),
	And(Reg, Reg, Reg),
	Andi(Reg, Reg, i32),
	Sll(Reg, Reg, Reg),
	Slli(Reg, Reg, i32),
	Srl(Reg, Reg, Reg),
	Srli(Reg, Reg, i32),
	Sra(Reg, Reg, Reg),
	Srai(Reg, Reg, i32),
	Mul(Reg, Reg, Reg),
	Div(Reg, Reg, Reg),
	Rem(Reg, Reg, Reg),
	Li(Reg, i32),
	La(Reg, String),
	Mv(Reg, Reg),
}

// this method can only be used without register allocation
fn opi(ins:&str, r1:&Reg, r2:&Reg, imm:&i32)->String{
	if *imm>=-2048&&*imm<=2047{
		format!("{ins} {r1}, {r2}, {imm}")
	}
	else {
		let s1 = format!("li {}, {}", Reg::T3, imm);
		let s2 = format!("{} {}, {}, {}", &ins[0..ins.len()-1], r1, r2, Reg::T3);
		format!("{}\n  {}", s1, s2)
	}
}

fn sli(ins:&str, r1:&Reg, r2:&Reg, imm:&i32)->String{
	if *imm>=-2048&&*imm<=2047{
		format!("{ins} {r1}, {imm}({r2})")
	}
	else {
		let s1 = format!("li {}, {}", Reg::T3, imm);
		let s2 = format!("add {}, {}, {}", Reg::T3, r2, Reg::T3);
		let s3 = format!("{} {}, 0({})", ins, r1, Reg::T3);
		format!("{}\n  {}\n  {}", s1, s2, s3)
	}
}

impl Display for Ins {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let s = match self {
			Ins::Beqz(rs, label) => format!("beqz {}, {}",rs, label),
			Ins::Bnez(rs, label) => format!("bnez {}, {}",rs, label),
			Ins::Beq(rs1, rs2, label) =>format!("beq {}, {}, {}", rs1, rs2, label),
			Ins::Bne(rs1, rs2, label) =>format!("bne {}, {}, {}", rs1, rs2, label),
			Ins::Blt(rs1, rs2, label) =>format!("blt {}, {}, {}", rs1, rs2, label),
			Ins::Bge(rs1, rs2, label) =>format!("bge {}, {}, {}", rs1, rs2, label),
			Ins::J(label) => format!("j {}", label),
			Ins::Call(label) => format!("call {}", label),
			Ins::Ret => format!("ret"),
			Ins::Lw(rs, rd, imm) => sli("lw", rs, rd, imm),
			Ins::Sw(rs1, rs2, imm) => sli("sw", rs1, rs2, imm),
			Ins::Add(rd, rs1, rs2) => format!("add {}, {}, {}", rd, rs1, rs2),
			Ins::Addi(rd, rs1, imm) => opi("addi", rd, rs1, imm),
			Ins::Sub(rd, rs1, rs2) => format!("sub {}, {}, {}", rd, rs1, rs2),
			Ins::Slt(rd, rs1, rs2) => format!("slt {}, {}, {}", rd, rs1, rs2),
			Ins::Sgt(rd, rs1, rs2) => format!("sgt {}, {}, {}", rd, rs1, rs2),
			Ins::Seqz(rd, rs) => format!("seqz {}, {}", rd, rs),
			Ins::Snez(rd, rs) => format!("snez {}, {}", rd, rs),
			Ins::Xor(rd, rs1, rs2) => format!("xor {}, {}, {}", rd, rs1, rs2),
			Ins::Xori(rd, rs1, imm) => opi("xori", rd, rs1, imm),
			Ins::Or(rd, rs1, rs2) => format!("or {}, {}, {}", rd, rs1, rs2),
			Ins::Ori(rd, rs1, imm) => opi("ori", rd, rs1, imm),
			Ins::And(rd, rs1, rs2) => format!("and {}, {}, {}", rd, rs1, rs2),
			Ins::Andi(rd, rs1, imm) => opi("andi", rd, rs1, imm),
			Ins::Sll(rd, rs1, rs2) => format!("sll {}, {}, {}", rd, rs1, rs2),
			Ins::Slli(rd, rs1, imm) => opi("slli", rd, rs1, imm),
			Ins::Srl(rd, rs1, rs2) => format!("srl {}, {}, {}", rd, rs1, rs2),
			Ins::Srli(rd, rs1, imm) => opi("srli", rd, rs1, imm),
			Ins::Sra(rd, rs1, rs2) => format!("sra {}, {}, {}", rd, rs1, rs2),
			Ins::Srai(rd, rs1, imm) => opi("srai", rd, rs1, imm),
			Ins::Mul(rd, rs1, rs2) => format!("mul {}, {}, {}", rd, rs1, rs2),
			Ins::Div(rd, rs1, rs2) => format!("div {}, {}, {}", rd, rs1, rs2),
			Ins::Rem(rd, rs1, rs2) => format!("rem {}, {}, {}", rd, rs1, rs2),
			Ins::Li(rd, imm) => format!("li {}, {}", rd, imm),
			Ins::La(rd, label) => format!("la {}, {}", rd, label),
			Ins::Mv(rd, rs) => format!("mv {}, {}", rd, rs),
		};
		write!(f, "  {}", s)
	}
}