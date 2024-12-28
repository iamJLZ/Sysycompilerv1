use std::fmt;

#[derive(Debug)]
pub enum Directive {
	Text,
	Data,
	Globl(String),
	Zero(usize),
	Word(Vec<i32>),
}

impl fmt::Display for Directive {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Directive::Text => write!(f, "  .text"),
			Directive::Data => write!(f, "  .data"),
			Directive::Globl(s) => write!(f, "  .globl {}", s),
			Directive::Word(v) => write!(f, "  .word {}", v.iter().map(i32::to_string).collect::<Vec<_>>().join(",")),
			Directive::Zero(i) => write!(f, "  .zero {}", i),
		}
	}
}