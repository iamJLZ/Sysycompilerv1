use pest::iterators::Pair;
use pest::Parser;

use super::ast::*;
use super::expr::parse_expr;
use super::symtab;
use super::symtab::FuncEntry;
use super::symtab::GVar;
use super::symtab::VarEntry;
use super::expr::Rule;
use super::expr::SysyParser;

fn parse_type(pair: Pair<Rule>) -> Result<symtab::Type, String> {
	match pair.as_str() {
		"void" => Ok(symtab::Type::Rtype(0)),
		"int" => Ok(symtab::Type::Rtype(1)),
		_ => Err(String::from("Unknown type:") + pair.as_str()),
	}
}

fn parse_stmt(pair: Pair<Rule>, gvar: &mut GVar) -> Result<Stmt, String> {
	match pair.into_inner().next() {
		Some(pa) => match pa.as_rule() {
			Rule::Return => {
				let mut pas = pa.into_inner();
				pas.next().unwrap();
				Ok(Stmt::Return(match pas.next() {
					Some(ep) => Some(Box::new(parse_expr(ep.into_inner(), gvar)?)),
					None => None,
				}))
			}
			Rule::Break => Ok(Stmt::Break),
			Rule::Block => Ok(Stmt::Block(Box::new(parse_block(pa, gvar)?))),
			Rule::Continue => Ok(Stmt::Continue),
			Rule::Expr => Ok(Stmt::Expr(Box::new(parse_expr(pa.into_inner(), gvar)?))),
			Rule::Loop => {
				let mut wl = pa.into_inner().next().unwrap().into_inner();
				let con = parse_expr(wl.next().unwrap().into_inner(), gvar)?;
				let stm = parse_stmt(wl.next().unwrap(), gvar)?;
				Ok(Stmt::Loop(Box::new(con), Box::new(stm)))
			}
			Rule::Branc => {
				let mut bl = pa.into_inner();
				let con = parse_expr(bl.next().unwrap().into_inner(), gvar)?;
				let pas = parse_stmt(bl.next().unwrap(), gvar)?;
				let fal = match bl.next() {
					Some(a) => Some(Box::new(parse_stmt(a, gvar)?)),
					None => None,
				};
				Ok(Stmt::Branc {
					cond: Box::new(con),
					pass: Box::new(pas),
					fail: fal,
				})
			}
			_ => unreachable!(),
		},
		None => Ok(Stmt::NONE),
	}
}

fn parse_block(pair: Pair<Rule>, gvar: &mut GVar) -> Result<Block, String> {
	gvar.enter();
	let res: Vec<BlockItem> = pair
		.into_inner()
		.map(|pa| -> Result<Vec<BlockItem>, String> {
			match pa.as_rule() {
				Rule::Stmt => Ok(vec![BlockItem::Stmt(parse_stmt(pa, gvar)?)]),
				Rule::VarDec => {
					let vars = parse_vardec(pa, gvar)?;
					let mut vds: Vec<BlockItem> = vec![];
					for i in vars {
						vds.push(BlockItem::Decl(i));
					}
					Ok(vds)
				}
				_ => unreachable!(),
			}
		})
		.collect::<Result<Vec<_>, _>>()?
		.into_iter()
		.flatten()
		.collect();
	gvar.exit();
	Ok(res)
}

fn parse_blockz(pair: Pair<Rule>, gvar: &mut GVar) -> Result<Block, String> {
	let res: Vec<BlockItem> = pair
		.into_inner()
		.map(|pa| -> Result<Vec<BlockItem>, String> {
			match pa.as_rule() {
				Rule::Stmt => Ok(vec![BlockItem::Stmt(parse_stmt(pa, gvar)?)]),
				Rule::VarDec => {
					let vars = parse_vardec(pa, gvar)?;
					let mut vds: Vec<BlockItem> = vec![];
					for i in vars {
						vds.push(BlockItem::Decl(i));
					}
					Ok(vds)
				}
				_ => unreachable!(),
			}
		})
		.collect::<Result<Vec<_>, _>>()?
		.into_iter()
		.flatten()
		.collect();
	Ok(res)
}

fn parse_params(pair: Pair<Rule>, gvar: &mut GVar) -> Result<Vec<VarEntry>, String> {
	let paras = pair.into_inner();
	let res = paras
		.map(|para| -> Result<VarEntry, String> {
			let mut ts = para.into_inner();
			let tp = parse_type(ts.next().unwrap())?;
			let sg = ts.next().unwrap().into_inner().next().unwrap();
			match sg.as_rule() {
				Rule::Name => {
					let rawname = sg.as_str();
					let idx = gvar.register_var(rawname, tp, None);
					Ok(gvar.getval(idx))
				}
				Rule::ListPara => {
					let mut sgs = sg.into_inner();
					let rawname =sgs.next().unwrap().as_str();
					let nsgs = sgs.skip(1);
					let mut subdim = nsgs.map(|p|->Result<usize,String>{
						if let Expr::Num(i) = parse_expr(p.into_inner(), gvar)?{
							Ok(i as usize)
						}
						else {
							unreachable!()
						}
					}).collect::<Result<Vec<_>,_>>()?;
					subdim.insert(0, 0);
					let ltp = symtab::Type::List(Box::new(tp), subdim);
					let idx = gvar.register_var(rawname, ltp, None);
					Ok(gvar.getval(idx))
				},
				_ => unreachable!(),
			}
		})
		.collect::<Result<_, _>>();
	res
}

fn parse_list(pair:Pair<Rule>, gvar: &mut GVar) -> Result<Vec<ListInit>,String>{
	pair.into_inner().map(|p|->Result<ListInit,String>{
		match p.as_rule() {
			Rule::Expr => Ok(ListInit::Expred(Box::new(parse_expr(p.into_inner(), gvar)?))),
			Rule::List => Ok(ListInit::Listed(parse_list(p, gvar)?)),
			_ => unreachable!(),
		}
	})
	.collect::<Result<Vec<ListInit>,String> >()
}

fn parse_funcdef(pair: Pair<Rule>, gvar: &mut GVar) -> Result<FuncDef, String> {
	gvar.enter();
	let mut pairs = pair.into_inner();
	let ft = parse_type(pairs.next().unwrap())?;
	let fname = pairs.next().unwrap().as_str();
	let fp = parse_params(pairs.next().unwrap(), gvar)?;
	let fe = FuncEntry::new(fname, ft, fp, false);
	let uid = gvar.register_func(fe);
	let bd = parse_blockz(pairs.next().unwrap(), gvar)?;
	gvar.exit();
	Ok(FuncDef {
		id: uid,
		body: Box::new(bd),
	})
}

fn parse_vardec(pair: Pair<Rule>, gvar: &mut GVar) -> Result<Vec<Decl>, String> {
	let mut ps = pair.into_inner();
	let tp: symtab::Type;
	let cot = ps.next().unwrap();
	let is_const = match cot.as_rule() {
		Rule::iconst => {
			tp = parse_type(ps.next().unwrap())?;
			true
		}
		Rule::Type => {
			tp = parse_type(cot)?;
			false
		}
		_ => unreachable!(),
	};
	let res = ps
		.map(|pa| -> Result<Decl, String> {
			match pa.as_rule() {
				Rule::Vardef => {
					let mut psa = pa.into_inner();
					let vname = psa.next().unwrap().as_str();
					if is_const {
						let val = match psa.next() {
							Some(a) => Some(if let Expr::Num(i) = parse_expr(a.into_inner(), gvar)?{
								i
							}
							else {
								unreachable!()
							}),
							None => Some(0),
						};
						gvar.register_var(vname, tp.clone(), val);
						Ok(Decl::VarsDec(VarInit::Constvar))
					} else {
						let idx = gvar.register_var(vname, tp.clone(), None);
						Ok(Decl::VarsDec(VarInit::VarDef(
							idx,
							Box::new(match psa.next() {
								Some(a) => parse_expr(a.into_inner(), gvar)?,
								None => Expr::Num(0),
							}),
						)))
					}
				}
				Rule::Listdef	=> {
					let mut psa = pa.into_inner();
					let vname = psa.next().unwrap().as_str();
					let dims:Vec<usize> = psa.next().unwrap().into_inner().map(|pa|->Result<usize,String>{
						if let Expr::Num(i) = parse_expr(pa.into_inner(), gvar)?{
							Ok(i as usize)
						}
						else {
							unreachable!()
						}
					}).collect::<Result<_,_>>()?;
					let initializer = match psa.next() {
						Some(rl) => FlatList::from_listinit(parse_list(rl, gvar)?, dims.as_slice())?,
						None => {
							if gvar.level()==1{
								FlatList::zero_new(dims.iter().product())
							}
							else {
								FlatList(vec![], dims.iter().product())
							}
						},
					};
					let idx = gvar.register_var(vname, symtab::Type::List(Box::new(tp.clone()), dims), None);
					Ok(Decl::VarsDec(VarInit::ListDef(idx, initializer)))
				}
				_ => unreachable!(),
			}
		})
		.collect::<Result<_, _>>();
	res
}

pub fn parse_file(file: &str, gvar: &mut GVar) -> Result<Comp, String> {
	let prog = SysyParser::parse(Rule::CompUnit, file)
		.unwrap()
		.next()
		.unwrap();
	let res: Vec<Decl> = prog
		.into_inner()
		.filter(|r|-> bool {
			!matches!(r.as_rule(), Rule::EOI)
		})
		.map(|pair| -> Result<Vec<Decl>, String> {
			match pair.as_rule() {
				Rule::FuncDef => Ok(vec![Decl::FuncDef(parse_funcdef(pair, gvar)?)]),
				Rule::VarDec => Ok(parse_vardec(pair, gvar)?),
				_ => unreachable!(),
			}
		})
		.collect::<Result<Vec<_>, _>>()?
		.into_iter()
		.flatten()
		.collect();
	Ok(res)
}
