use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_derive::Parser;

use super::ast::Expr;
use super::symtab::{GVar, NType};

pub fn parse_expr(pairs: Pairs<Rule>, gvar: &GVar) -> Result<Expr, String> {
	let pratt = PrattParser::new()
		.op(Op::infix(Rule::asg, Assoc::Right)
			| Op::infix(Rule::addasg, Assoc::Right)
			| Op::infix(Rule::subasg, Assoc::Right)
			| Op::infix(Rule::mulasg, Assoc::Right)
			| Op::infix(Rule::divasg, Assoc::Right)
			| Op::infix(Rule::modasg, Assoc::Right)
			| Op::infix(Rule::xorasg, Assoc::Right)
			| Op::infix(Rule::bitandasg, Assoc::Right)
			| Op::infix(Rule::bitorasg, Assoc::Right)
			| Op::infix(Rule::shlasg, Assoc::Right)
			| Op::infix(Rule::shrasg, Assoc::Right))
		.op(Op::infix(Rule::logor, Assoc::Left))
		.op(Op::infix(Rule::logand, Assoc::Left))
		.op(Op::infix(Rule::bitor, Assoc::Left))
		.op(Op::infix(Rule::xor, Assoc::Left))
		.op(Op::infix(Rule::bitand, Assoc::Left))
		.op(Op::infix(Rule::eq, Assoc::Left) | Op::infix(Rule::neq, Assoc::Left))
		.op(Op::infix(Rule::geq, Assoc::Left)
			| Op::infix(Rule::leq, Assoc::Left)
			| Op::infix(Rule::grt, Assoc::Left)
			| Op::infix(Rule::les, Assoc::Left))
		.op(Op::infix(Rule::shl, Assoc::Left) | Op::infix(Rule::shr, Assoc::Left))
		.op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
		.op(Op::infix(Rule::mul, Assoc::Left)
			| Op::infix(Rule::div, Assoc::Left)
			| Op::infix(Rule::moo, Assoc::Left))
		.op(Op::prefix(Rule::neg)
			| Op::prefix(Rule::pos)
			| Op::prefix(Rule::not)
			| Op::prefix(Rule::bitnot));

	pratt
		.map_primary(|primary| -> Result<Expr, String> {
			match primary.as_rule() {
				Rule::num_bin => Ok(Expr::Num(i32::from_str_radix(&primary.as_str()[2..], 2).unwrap())),
				Rule::num_oct => Ok(Expr::Num(i32::from_str_radix(&primary.as_str()[1..], 8).unwrap())),
				Rule::num_dec => Ok(Expr::Num(i32::from_str_radix(&primary.as_str(), 10).unwrap())),
				Rule::num_hex => Ok(Expr::Num(i32::from_str_radix(&primary.as_str()[2..], 16).unwrap())),
				Rule::Expr => Ok(parse_expr(primary.into_inner(), gvar)?),
				Rule::Name => {
					let idx = gvar.lookup(primary.as_str().to_string(), NType::Varn)?;
					match gvar.getvarval(idx) {
						Some(a) => Ok(Expr::Num(a)),
						None => Ok(Expr::Var(idx)),
					}
				}
				Rule::IndexCall => {
					let mut ic = primary.into_inner();
					let idx = gvar.lookup(ic.next().unwrap().as_str().to_string(), NType::Varn)?;
					Ok(Expr::IndexCall(
						idx,
						ic.map(|p| -> Result<Expr, String> { parse_expr(p.into_inner(), gvar) })
							.collect::<Result<_, _>>()?,
					))
				}
				Rule::FuncCall => {
					let mut ic = primary.into_inner();
					let idx = gvar.lookup(ic.next().unwrap().as_str().to_string(), NType::Funcn)?;
					Ok(Expr::FuncCall(
						idx,
						ic.next()
							.unwrap()
							.into_inner()
							.map(|p| -> Result<Expr, String> { parse_expr(p.into_inner(), gvar) })
							.collect::<Result<_, _>>()?,
					))
				}
				_ => Err(format!("{:?}", primary)),
			}
		})
		.map_prefix(|op, rhs| -> Result<Expr, String> {
			match rhs.clone()? {
				Expr::Num(a) => {
					return Ok(Expr::Num(match op.as_rule() {
						Rule::neg => -a,
						Rule::pos => a,
						Rule::not => (a==0)as i32,
						Rule::bitnot=> !a,
						_ => unreachable!(),
					}))
				},
				_ => {},
			}
			match op.as_rule() {
				Rule::neg => Ok(Expr::Neg(Box::new(rhs?))),
				Rule::pos => Ok(Expr::Pos(Box::new(rhs?))),
				Rule::not => Ok(Expr::Not(Box::new(rhs?))),
				Rule::bitnot => Ok(Expr::Bitnot(Box::new(rhs?))),
				_ => unreachable!(), // TODO
			}
		})
		// .map_postfix(|lhs, op|->Result<Expr, String> {match op.as_rule() {
		// 	_	=> unreachable!(),	// TODO
		// }
		// })
		.map_infix(|lhs, op, rhs| -> Result<Expr, String> {
			match (lhs.clone()?,rhs.clone()?) {
				(Expr::Num(a),Expr::Num(b)) => {return Ok(Expr::Num(match op.as_rule() {
					Rule::logand=>((a != 0) && (b != 0)) as i32,
					Rule::logor =>((a != 0) || (b != 0)) as i32,
					Rule::bitor =>a | b,
					Rule::xor =>a ^ b,
					Rule::bitand=> a & b,
					Rule::neq =>(a != b) as i32,
					Rule::eq => (a == b) as i32,
					Rule::leq =>(a <= b) as i32,
					Rule::geq =>(a >= b) as i32,
					Rule::les =>(a < b) as i32,
					Rule::grt =>(a > b) as i32,
					Rule::shr =>a >> b,
					Rule::shl =>((a as u32) << b) as i32,
					Rule::sub =>a - b,
					Rule::add =>a + b,
					Rule::moo =>a % b,
					Rule::div =>a / b,
					Rule::mul =>a * b,
					_ => unreachable!(),
				}))},
				_ => {},
			}
			match op.as_rule() {
				Rule::asg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> Ok(Expr::Asg(Box::new(lhs?), Box::new(rhs?))),
					_ => Err(String::from("= left operator number must be left value")),
				},
				Rule::addasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> {
						if let Expr::Num(0) = rhs.clone()? {
							Ok(Expr::Asg(Box::new(lhs?), Box::new(rhs?)))
						}else {
							Ok(Expr::Addasg(Box::new(lhs?), Box::new(rhs?)))	
						}
					},
					_ => Err(String::from("+= left operator number must be left value")),
				},
				Rule::subasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> {
						if let Expr::Num(0) = rhs.clone()? {
							Ok(Expr::Asg(Box::new(lhs?), Box::new(rhs?)))
						}else {
							Ok(Expr::Subasg(Box::new(lhs?), Box::new(rhs?)))	
						}
					},
					_ => Err(String::from("-= left operator number must be left value")),
				},
				Rule::mulasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(0) => Ok(Expr::Asg(Box::new(lhs?), Box::new(Expr::Num(0)))),
						Expr::Num(1) => lhs,
						_ => Ok(Expr::Mulasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from("*= left operator number must be left value")),
				},
				Rule::divasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(1) => lhs,
						_ => Ok(Expr::Divasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from("/= left operator number must be left value")),
				},
				Rule::modasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(1) => Ok(Expr::Asg(Box::new(lhs?), Box::new(Expr::Num(0)))),
						_ => Ok(Expr::Modasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from("%= left operator number must be left value")),
				},
				Rule::shlasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(0) => lhs,
						Expr::Num(i) if i>=32 => Ok(Expr::Asg(Box::new(lhs?), Box::new(Expr::Num(0)))),
						_ => Ok(Expr::Shlasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from("<<= left operator number must be left value")),
				},
				Rule::shrasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(0) => lhs,
						Expr::Num(i) if i>=32 => Ok(Expr::Asg(Box::new(lhs?), Box::new(Expr::Num(0)))),
						_ => Ok(Expr::Shrasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from(">>= left operator number must be left value")),
				},
				Rule::xorasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(0) => lhs,
						_ => Ok(Expr::Xorasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from("^= left operator number must be left value")),
				},
				Rule::bitandasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(-1) => lhs,
						Expr::Num(0) => Ok(Expr::Asg(Box::new(lhs?), Box::new(Expr::Num(0)))),
						_ => Ok(Expr::Bitandasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from("&= left operator number must be left value")),
				},
				Rule::bitorasg => match lhs.clone()? {
					Expr::Var(_) | Expr::IndexCall(_,_)=> match rhs.clone()? {
						Expr::Num(0) => lhs,
						Expr::Num(-1) => Ok(Expr::Asg(Box::new(lhs?), Box::new(Expr::Num(-1)))),
						_ => Ok(Expr::Bitorasg(Box::new(lhs?), Box::new(rhs?))),
					},
					_ => Err(String::from("|= left operator number must be left value")),
				},
				Rule::logand => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(0),_) | (_, Expr::Num(0)) => Ok(Expr::Num(0)),
					(Expr::Num(i),_) if i!=0 => rhs,
					(_, Expr::Num(i)) if i!=0 => lhs,
					_ => Ok(Expr::Logand(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::logor => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(i),_) if i!=0 => Ok(Expr::Num(1)),
					(_, Expr::Num(j)) if j!=0 => Ok(Expr::Num(1)),
					(Expr::Num(0),_) => rhs,
					(_, Expr::Num(0)) => lhs,
					_ => Ok(Expr::Logor(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::bitor => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(-1),_) | (_, Expr::Num(-1))=> Ok(Expr::Num(-1)),
					(Expr::Num(0),_) => rhs,
					(_, Expr::Num(0)) => lhs,
					_ => Ok(Expr::Bitor(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::xor => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(0),_) => rhs,
					(_, Expr::Num(0)) => lhs,
					_ => Ok(Expr::Xor(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::bitand => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(0),_) | (_, Expr::Num(0))=> Ok(Expr::Num(0)),
					(Expr::Num(-1),_) => rhs,
					(_, Expr::Num(-1)) => lhs,
					_ => Ok(Expr::Bitand(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::neq => Ok(Expr::Neq(Box::new(lhs?), Box::new(rhs?))),
				Rule::eq => Ok(Expr::Eq(Box::new(lhs?), Box::new(rhs?))),
				Rule::leq => Ok(Expr::Leq(Box::new(lhs?), Box::new(rhs?))),
				Rule::geq => Ok(Expr::Geq(Box::new(lhs?), Box::new(rhs?))),
				Rule::les => Ok(Expr::Les(Box::new(lhs?), Box::new(rhs?))),
				Rule::grt => Ok(Expr::Grt(Box::new(lhs?), Box::new(rhs?))),
				Rule::sar => match (lhs.clone()?, rhs.clone()?){
					(_, Expr::Num(0)) => lhs,
					(Expr::Num(0),_) => Ok(Expr::Num(0)),
					(Expr::Num(-1),_) => Ok(Expr::Num(-1)),
					_ => Ok(Expr::Sar(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::shr => match (lhs.clone()?, rhs.clone()?){
					(_, Expr::Num(0)) => lhs,
					(_, Expr::Num(i)) if i>=32 => Ok(Expr::Num(0)),
					(Expr::Num(0),_) => Ok(Expr::Num(0)),
					_ => Ok(Expr::Shr(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::shl => match (lhs.clone()?, rhs.clone()?){
					(_, Expr::Num(0)) => lhs,
					(_, Expr::Num(i)) if i>=32 => Ok(Expr::Num(0)),
					(Expr::Num(0),_) => Ok(Expr::Num(0)),
					_ => Ok(Expr::Shl(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::sub => match (lhs.clone()?, rhs.clone()?){
					(_, Expr::Num(0)) => lhs,
					_ => Ok(Expr::Sub(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::add => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(0),_) => rhs,
					(_, Expr::Num(0)) => lhs,
					_ => Ok(Expr::Add(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::moo => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(0),_) | (_, Expr::Num(1))=> Ok(Expr::Num(0)),
					_ => Ok(Expr::Moo(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::div => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(0),_) => Ok(Expr::Num(0)),
					(_, Expr::Num(1)) => lhs,
					_ => Ok(Expr::Div(Box::new(lhs?), Box::new(rhs?)))
				},
				Rule::mul => match (lhs.clone()?, rhs.clone()?){
					(Expr::Num(0),_) | (_, Expr::Num(0))=> Ok(Expr::Num(0)),
					(Expr::Num(1),_) => rhs,
					(_, Expr::Num(1)) => lhs,
					_ => Ok(Expr::Mul(Box::new(lhs?), Box::new(rhs?)))
				},
				_ => unreachable!(), // TODO
			}
		})
		.parse(pairs)
}

#[derive(Parser)]
#[grammar = "frontend/sysy.pest"]
pub struct SysyParser;
