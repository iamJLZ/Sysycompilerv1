use std::usize;

use super::ast;
use super::symtab;
fn ir_alloc(name: String, typ: String) -> String {
	format!("{} = alloc {}\n", name, typ)
}

fn ir_load(src: String, dst: String) -> String {
	format!("{} = load {}\n", dst, src)
}

fn ir_store(src: String, dst: String) -> String {
	format!("store {},{}\n", src, dst)
}

fn ir_br(val:String, dst1:&String, dst2:&String)->String{
	format!("br {val},{dst1},{dst2}\n")
}

fn ir_jump(dst:&String)->String{
	format!("jump {dst}\n")
}

fn ir_getptr(dst:&String, base:&String, offset:&String)->String{
	format!("{} = getptr {}, {}\n", dst, base, offset)
}

fn ir_getelemptr(dst:&String, base:&String, offset:&String)->String{
	format!("{} = getelemptr {}, {}\n", dst, base, offset)
}
enum Bop {
	Sar,
	Bitor,
	Xor,
	Bitand,
	Neq,
	Eq,
	Leq,
	Geq,
	Les,
	Grt,
	Shr,
	Shl,
	Sub,
	Add,
	Moo,
	Mul,
	Div,
}

fn ir_binop(op: Bop, lhs: String, rhs: String, dst: String) -> String {
	match op {
		Bop::Sar => format!("{} = sar {},{}\n", dst, lhs, rhs),
		Bop::Bitor => format!("{} = or {},{}\n", dst, lhs, rhs),
		Bop::Xor => format!("{} = xor {},{}\n", dst, lhs, rhs),
		Bop::Bitand => format!("{} = and {},{}\n", dst, lhs, rhs),
		Bop::Neq => format!("{} = ne {},{}\n", dst, lhs, rhs),
		Bop::Eq => format!("{} = eq {},{}\n", dst, lhs, rhs),
		Bop::Leq => format!("{} = le {},{}\n", dst, lhs, rhs),
		Bop::Geq => format!("{} = ge {},{}\n", dst, lhs, rhs),
		Bop::Les => format!("{} = lt {},{}\n", dst, lhs, rhs),
		Bop::Grt => format!("{} = gt {},{}\n", dst, lhs, rhs),
		Bop::Shr => format!("{} = shr {},{}\n", dst, lhs, rhs),
		Bop::Shl => format!("{} = shl {},{}\n", dst, lhs, rhs),
		Bop::Sub => format!("{} = sub {},{}\n", dst, lhs, rhs),
		Bop::Add => format!("{} = add {},{}\n", dst, lhs, rhs),
		Bop::Moo => format!("{} = mod {},{}\n", dst, lhs, rhs),
		Bop::Mul => format!("{} = mul {},{}\n", dst, lhs, rhs),
		Bop::Div => format!("{} = div {},{}\n", dst, lhs, rhs),
	}
}

enum TIdent {
	Basicblock,
	Variable,
}

fn temp_ident(ti: TIdent, gvar: &mut symtab::GVar) -> String {
	match ti {
		TIdent::Basicblock => format!("%b{}", gvar.require_index()),
		TIdent::Variable => format!("%v{}", gvar.require_index()),
	}
}

fn getvarname(idx: usize, gvar: &symtab::GVar) -> String {
	format!("@{}", gvar.vartab[idx].name)
}

fn getfuncname(idx: usize, gvar: &symtab::GVar) -> String {
	format!("@{}", gvar.functab[idx].name)
}

fn type_ir(tp: &symtab::Type, gvar: &symtab::GVar) -> String {
	match tp {
		symtab::Type::Rtype(a) => gvar.typetab[*a].name.clone(),
		symtab::Type::List(t, s) => s.iter().fold(type_ir(&**t, gvar), |acc, &x|->String{
			format!("[{acc},{x}]")
		}),
		symtab::Type::Pointer(tp) => format!("*{}", type_ir(&(**tp), gvar)),
	}
}

fn params_ir(params: &Vec<symtab::VarEntry>, gvar: &symtab::GVar) -> (String, String) {
	let ss = params
		.iter()
		.map(|ve| -> (String, String) {
			let strname = &(*ve).name;
			let strtype = 
			match &ve.typ {
				symtab::Type::List(bt, _)=> format!("*{}",type_ir(&*bt, gvar)),
				_ => type_ir(&ve.typ, gvar),
			};
			(format!(
				"{}:{}",
				format!("%{}", strname),
				strtype
			),
			format!("@{} = alloc {}\nstore %{}, @{}\n", strname, strtype, strname, strname))
		})
		.collect::<Vec<(String, String)>>();
	let (ps, ls):(Vec<_>, Vec<_>) = ss.iter().cloned().unzip();
	(ps.join(","), ls.join(""))
}

fn vardec_ir(vd: &ast::VarInit, gvar: &mut symtab::GVar) -> (String, String) {
	match vd {
		ast::VarInit::Constvar => (String::new(), String::new()),
		ast::VarInit::VarDef(idx, ini) => {
			let (s1, opn1) = expr_ir(&(**ini), gvar);
			let opn2 = getvarname(*idx, gvar);
			let s2 = ir_alloc(opn2.clone(), type_ir(&gvar.vartab[*idx].typ, gvar));
			let s3 = ir_store(opn1, opn2.clone());
			(format!("{}{}{}", s1, s2, s3), opn2)
		},
		ast::VarInit::ListDef(idx, ini) => {
			let vtype = if let symtab::Type::List(a, _) = gvar.getval(*idx).typ{
				symtab::Type::List(a, vec![ini.1])
			}
			else {unreachable!()};
			let name = getvarname(*idx, gvar);
			let alc = ir_alloc(name.clone(), type_ir(&vtype, gvar));
			let mut offset = 0;
			let sinit = ini.0.iter().map(|fl|->String{
				match fl {
					ast::FlatListInit::Zeros(n) => {
						let mut s:Vec<String> = vec![];
						for _i in 0..*n{
							let opn0 = temp_ident(TIdent::Variable, gvar);
							let sget = ir_getelemptr(&opn0, &name, &offset.to_string());
							let sstr = ir_store(0.to_string(), opn0);
							s.push(format!("{sget}{sstr}"));
							offset+=1;
						}
						s.join("")
					},
					ast::FlatListInit::Nonzero(a) =>{
						a.iter().map(|i|->String{
							let opn0 = temp_ident(TIdent::Variable, gvar);
							let sget = ir_getelemptr(&opn0, &name, &offset.to_string());
							let sstr = ir_store(i.to_string(), opn0);
							offset+=1;
							format!("{sget}{sstr}")
						}).collect::<Vec<_>>().join("")
					},
					ast::FlatListInit::Notnum(ev)=>{
						ev.iter().map(|e|->String{
							let (s1, opn1) = expr_ir(e, gvar);
							let opn0 = temp_ident(TIdent::Variable, gvar);
							let sget = ir_getelemptr(&opn0, &name, &offset.to_string());
							let sstr = ir_store(opn1, opn0);
							offset+=1;
							format!("{s1}{sget}{sstr}")
						}).collect::<Vec<_>>().join("")
					},
				}
			}).collect::<Vec<String>>().join("");
			(format!("{alc}{sinit}\n"),name)
		}
	}
}

fn stmt_ir(stmt:&ast::Stmt, gvar:&mut symtab::GVar)->String{
	match stmt {
		ast::Stmt::Block(c) => block_ir(c, gvar),
		ast::Stmt::Expr(d) => expr_ir(d, gvar).0,
		ast::Stmt::NONE => String::new(),
		ast::Stmt::Return(e) => {
			let (s1, opn1) = match e {
				Some(ex) => expr_ir(&(**ex), gvar),
				None => (String::new(), String::new()),
			};
			format!("{}ret {}\n", s1, opn1)
		}
		ast::Stmt::Branc{ cond, pass, fail } => {
			let bt = temp_ident(TIdent::Basicblock, gvar);
			let bn = temp_ident(TIdent::Basicblock, gvar);
			let passir= stmt_ir(&**&pass, gvar);
			let btc = format!("{}:\n{}{}", bt, passir, ir_jump(&bn));
			let (bf, bfc, bnc) = match fail {
				Some(a) => {
					let bff = temp_ident(TIdent::Basicblock, gvar);
					let bfca = stmt_ir(&*a, gvar);
					(bff.clone(), format!("{}:\n{}{}",bff,bfca,ir_jump(&bn)),format!("{}:\n",bn))
				},
				None => (bn.clone(), String::new(), format!("{}:\n",bn)),
			};
			match *cond.clone() {
				ast::Expr::Logand(lhs, rhs) => {
					let shortcir = short(true, &lhs, &rhs, gvar, &bt, &bf);
					format!("{}{}{}{}",shortcir,btc,bfc,bnc)
				},
				ast::Expr::Logor(lhs, rhs) => {
					let shortcir = short(false, &lhs, &rhs, gvar, &bt, &bf);
					format!("{}{}{}{}",shortcir,btc,bfc,bnc)
				},
				_ => {
					let (str1, tv1) = expr_ir(&**cond, gvar);
					let jud = ir_br(tv1, &bt, &bf);
					format!("{}{}{}{}{}", str1, jud, btc, bfc, bnc)
				}
			}
		},
		ast::Stmt::Loop(cond, body) => {
			gvar.enter_loop();
			let bj = temp_ident(TIdent::Basicblock, gvar);	// before loop body
			let bn = temp_ident(TIdent::Basicblock, gvar);	// next
			let bt = temp_ident(TIdent::Basicblock, gvar);	// loop body
			let body = format!("{}:\n{}\njump {}\n", bt, stmt_ir(&**body, gvar), bj);
			gvar.loopb.pop();
			match *cond.clone() {
				ast::Expr::Logand(lhs, rhs) => {
					let shortcir = short(true, &lhs, &rhs, gvar, &bt, &bn);
					format!("{}:\n{}{}{}:\n", bj, shortcir, body, bn)
				},
				ast::Expr::Logor(lhs, rhs) => {
					let shortcir = short(false, &lhs, &rhs, gvar, &bt, &bn);
					format!("{}:\n{}{}{}:\n", bj, shortcir, body, bn)
				},
				_ => {
					let (str1, tv1) = expr_ir(&**cond, gvar);
					let jud = ir_br(tv1, &bt, &bn);
					format!("{}:\n{}{}{}{}:\n", bj, str1, jud, body, bn)
				}
			}
		},
		ast::Stmt::Break => {
			let (_, e) = gvar.loopb.last().unwrap();
			format!("jump %b{}\n",e)
		},
		ast::Stmt::Continue => {
			let (b, _) = gvar.loopb.last().unwrap();
			format!("jump %b{}\n",b)
		},
	}
}

fn block_ir(block: &ast::Block, gvar: &mut symtab::GVar) -> String {
	block.iter().map(|bi|->String{
		match bi {
			ast::BlockItem::Decl(a) => match a {
				ast::Decl::VarsDec(b) => vardec_ir(b, gvar).0,
				_ => unreachable!(),
			},
			ast::BlockItem::Stmt(b) => stmt_ir(b, gvar),
		}
	}).collect::<String>()
}

fn short(is_and:bool, lhs:&ast::Expr, rhs:&ast::Expr, gvar:&mut symtab::GVar, btrue:&String, bfalse:&String)->String{
	let (str1,tv1) = expr_ir(lhs, gvar);
	let (str2,tv2) = expr_ir(rhs, gvar);
	let b1 = temp_ident(TIdent::Basicblock, gvar);
	let jud = match is_and {
		true => ir_br(tv1, &b1, bfalse),
		false => ir_br(tv1, btrue, &b1),
	};
	let b1b = match is_and {
		true => ir_br(tv2, btrue, bfalse),
		false => ir_br(tv2, btrue, bfalse),
	};
	format!("{str1}{jud}{b1}:\n{str2}{b1b}")
}

fn left_val(expr:&ast::Expr,gvar:&mut symtab::GVar)->(String, String){
	match expr {
		ast::Expr::Var(a) => (String::new(), getvarname(*a, gvar)),
		ast::Expr::IndexCall(idx, params) => {
			let mut paramlist = params.clone();
			let aslist = gvar.getval(*idx);
			match aslist.typ {
				symtab::Type::List(_, dims)=>{
					let mut bas = 1;
					let mut numoff = 0;
					let mut ev:Vec<ast::Expr> = vec![];
					paramlist.resize(dims.len(), ast::Expr::Num(0));
					let pd = paramlist.iter().zip(dims.iter()).rev();
					for (ex, dx) in pd{
						match ex {
							ast::Expr::Num(n) => numoff+=n*bas,
							_  => ev.push(ast::Expr::Mul(Box::new(ast::Expr::Num(bas)), Box::new((*ex).clone()))),
						}
						bas*=*dx as i32;
					}
					let flatoff = ev.iter().fold(ast::Expr::Num(numoff), |acc, x|->ast::Expr{
						ast::Expr::Add(Box::new(acc), Box::new((*x).clone()))
					});
					let (s1, opn1) = expr_ir(&flatoff, gvar);
					let opn2 = temp_ident(TIdent::Variable, gvar);
					let s2 = if dims[0]==0{
						let opn3= temp_ident(TIdent::Variable, gvar);
						let loads = ir_load(getvarname(*idx, gvar), opn3.clone());
						let gets = ir_getptr(&opn2, &opn3, &opn1);
						format!("{loads}{gets}")
					}
					else{
						ir_getelemptr(&opn2, &getvarname(*idx, gvar), &opn1)
					} ;
					(format!("{s1}{s2}"),opn2)
				},
				//symtab::Type::Pointer(_bt)=> unreachable!(),
				_ => unreachable!(),
			}
		}
		_ => unreachable!(),
	}
}

fn expr_ir(expr: &ast::Expr, gvar: &mut symtab::GVar) -> (String, String) {
	match expr.clone() {
		ast::Expr::Num(a) => (String::new(), a.to_string()),
		ast::Expr::Var(a) => {
			let tv0 = temp_ident(TIdent::Variable, gvar);
			let vari = gvar.getval(a);
			match vari.typ {
				symtab::Type::Rtype(_) => (ir_load(getvarname(a, gvar), tv0.clone()),tv0),
				symtab::Type::List(_, dims) => {
					if dims[0]==0 {
						(ir_load(getvarname(a, gvar), tv0.clone()),tv0)
					}
					else{
						(ir_getelemptr(&tv0, &getvarname(a, gvar),&0.to_string()),tv0)
					}
				}
				_ => unreachable!(),
			}
		},
		ast::Expr::Asg(lhs, rhs) => {
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			(format!("{}{}{}", s0, s1, ir_store(opn2, opn1.clone())), opn1)
		},
		ast::Expr::Addasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Add, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Bitandasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Bitand, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Bitorasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Bitor, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Divasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Div, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Modasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Moo, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Mulasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Mul, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Shlasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Shl, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Shrasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Shr, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Subasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Sub, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Xorasg(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s0, opn1) = left_val(&(**&lhs), gvar);
			let (s1, opn2) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Xor, opn1.clone(), opn2, tv1.clone());
			let s3 = ir_store(tv1, opn1.clone());
			(format!("{}{}{}{}", s0, s1, s2, s3), opn1)
		},
		ast::Expr::Logand(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let bt = temp_ident(TIdent::Basicblock, gvar);
			let bf = temp_ident(TIdent::Basicblock, gvar);
			let bn = temp_ident(TIdent::Basicblock, gvar);
			let btc = format!("{}:\nstore 1, @vmid\n{}",bt, ir_jump(&bn));
			let bfc = format!("{}:\nstore 0, @vmid\n{}",bf, ir_jump(&bn));
			let bnc = format!("{}:\n{} = load @vmid\n",bn, tv1);
			(format!("{}{}{}{}" ,short(true, &*lhs, &*rhs, gvar, &bt, &bf),btc, bfc, bnc),tv1)
		},
		ast::Expr::Logor(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let bt = temp_ident(TIdent::Basicblock, gvar);
			let bf = temp_ident(TIdent::Basicblock, gvar);
			let bn = temp_ident(TIdent::Basicblock, gvar);
			let btc = format!("{}:\nstore 1, @vmid\n{}",bt, ir_jump(&bn));
			let bfc = format!("{}:\nstore 0, @vmid\n{}",bf, ir_jump(&bn));
			let bnc = format!("{}:\n{} = load @vmid\n",bn, tv1);
			(format!("{}{}{}{}" ,short(false, &*lhs, &*rhs, gvar, &bt, &bf),btc, bfc, bnc),tv1)
		},
		ast::Expr::Bitor(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Bitor, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Xor(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Xor, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Bitand(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Bitand, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Neq(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Neq, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Eq(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Eq, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Leq(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Leq, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Les(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Les, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Geq(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Geq, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Grt(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Grt, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Sar(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Sar, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Shr(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Shr, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Shl(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Shl, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Sub(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Sub, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Add(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Add, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Moo(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Moo, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Mul(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Mul, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Div(lhs, rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&lhs), gvar);
			let (s2, opn2) = expr_ir(&(**&rhs), gvar);
			let s3 = ir_binop(Bop::Div, opn1, opn2, tv1.clone());
			(format!("{}{}{}", s1, s2, s3), tv1)
		},
		ast::Expr::Neg(rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Sub, String::from("0"), opn1, tv1.clone());
			(format!("{}{}", s1, s2), tv1)
		},
		ast::Expr::Pos(rhs) => expr_ir(&(**&rhs), gvar),
		ast::Expr::Not(rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Eq, String::from("0"), opn1, tv1.clone());
			(format!("{}{}", s1, s2), tv1)
		},
		ast::Expr::Bitnot(rhs) => {
			let tv1 = temp_ident(TIdent::Variable, gvar);
			let (s1, opn1) = expr_ir(&(**&rhs), gvar);
			let s2 = ir_binop(Bop::Xor, String::from("-1"), opn1, tv1.clone());
			(format!("{}{}", s1, s2), tv1)
		},
		ast::Expr::FuncCall(idx, paramlist) => {
			let functp = gvar.getfunc(idx).rettype.clone();
			let funcname = getfuncname(idx, gvar);
			let (exprs, realp):(Vec<_>, Vec<_>) = paramlist.iter().map(|ep|{expr_ir(ep, gvar)}).collect::<Vec<_>>().iter().cloned().unzip();
			let exps2 = exprs.join("");
			let callexpr = format!("call {}({})", funcname, realp.join(","));
			match functp {
				symtab::Type::Rtype(0) => {
					(format!("{}{}\n", exps2, callexpr), String::new())
				}
				symtab::Type::List(_,_) => unreachable!(),
				_ => {
					let v1 = temp_ident(TIdent::Variable, gvar);
					(format!("{}{} = {}\n",exps2, v1, callexpr), v1)
				}
			}
		},
		ast::Expr::IndexCall(idx, mut paramlist)=>{
			let aslist = gvar.getval(idx);
			match aslist.typ {
				symtab::Type::List(_, dims)=>{
					let mut bas = 1;
					let mut numoff = 0;
					let mut ev:Vec<ast::Expr> = vec![];
					let is_ptr = paramlist.len() < dims.len();
					paramlist.resize(dims.len(), ast::Expr::Num(0));
					let pd = paramlist.iter().zip(dims.iter()).rev();
					for (ex, dx) in pd{
						match ex {
							ast::Expr::Num(n) => numoff+=n*bas,
							_  => ev.push(ast::Expr::Mul(Box::new(ast::Expr::Num(bas)), Box::new((*ex).clone()))),
						}
						bas*=*dx as i32;
					}
					let flatoff = ev.iter().fold(ast::Expr::Num(numoff), |acc, x|->ast::Expr{
						ast::Expr::Add(Box::new(acc), Box::new((*x).clone()))
					});
					let (s1, opn1) = expr_ir(&flatoff, gvar);
					let opn2 = temp_ident(TIdent::Variable, gvar);
					let s2 = if dims[0]==0{
						let opn4= temp_ident(TIdent::Variable, gvar);
						let loads = ir_load(getvarname(idx, gvar), opn4.clone());
						let gets = ir_getptr(&opn2, &opn4, &opn1);
						format!("{loads}{gets}")
					}
					else{
						ir_getelemptr(&opn2, &getvarname(idx, gvar), &opn1)
					} ;
					if is_ptr{
						(format!("{s1}{s2}"),opn2)
					}
					else{
						let opn3 = temp_ident(TIdent::Variable, gvar);
						let s3 = ir_load(opn2, opn3.clone());
						(format!("{s1}{s2}{s3}"), opn3)
					}
				},
				//symtab::Type::Pointer(_bt)=> unreachable!(),
				_ => unreachable!(),
			}
		}
	}
}

fn func_ir(funcdef: &ast::FuncDef, gvar: &mut symtab::GVar) -> String {
	let func = &gvar.functab[funcdef.id];
	let midv = "@vmid = alloc i32\n";
	let (parair, loadir) = params_ir(&func.params, gvar);
	format!(
		"fun {}({}){}{{\n{}:\n{}{}{}}}\n",
		format!("@{}", func.name),
		parair,
		match func.rettype {
			symtab::Type::Rtype(0) => String::new(),
			_ => format!(":{}", type_ir(&func.rettype, gvar)),
		},
		temp_ident(TIdent::Basicblock, gvar),
		loadir,
		midv,
		block_ir(&(*funcdef.body), gvar)
	)
}

pub fn prog_ir(comp: &ast::Comp, gvar: &mut symtab::GVar) -> String {
	let predecl = "decl @getint(): i32\ndecl @getch(): i32\ndecl @getarray(*i32): i32\ndecl @putint(i32)\ndecl @putch(i32)\ndecl @putarray(i32, *i32)\ndecl @starttime()\ndecl @stoptime()\n";
	// let predecl = "";
	gvar.reset_index();
	let s = comp
		.iter()
		.map(|d| -> String {
			match d {
				ast::Decl::FuncDef(fd) => func_ir(fd, gvar),
				ast::Decl::VarsDec(vi) => {
					match vi {
						ast::VarInit::Constvar => String::new(),
						ast::VarInit::VarDef(idx, init) =>{
							let (_, val) = expr_ir(&init, gvar);
							let vname = getvarname(*idx, gvar);
							format!("global {} = alloc i32, {}\n", vname, val)
						}
						ast::VarInit::ListDef(idx, initializer) => {
							let aggre = if let (ast::FlatListInit::Zeros(_), 1) = (&initializer.0[0],initializer.0.len()){
								String::from("zeroinit")
							}
							else{
								format!("{{{}}}",initializer.0.iter().map(|p|->String{
									match p {
										ast::FlatListInit::Zeros(n) => {
											vec!["0";*n].join(",")
										},
										ast::FlatListInit::Nonzero(a) =>{
											a.iter().map(i32::to_string).collect::<Vec<_>>().join(",")
										}
										_=> unreachable!(),
									}
								}).collect::<Vec<String>>().join(","))
							};
							let vname = getvarname(*idx, gvar);
							let vtype = if let symtab::Type::List(a,_) = gvar.getval(*idx).typ{
								symtab::Type::List(a, vec![initializer.1])
							}
							else {unreachable!()};
							format!("global {} = alloc {}, {}\n",vname, type_ir(&vtype, gvar), aggre)
						}
					}
				} 
			}
		})
		.collect::<String>();
	let mut state = false;
	let mut sv2:Vec<String> = vec![];
	for val in s.lines(){
		let valp = val.trim();
		if valp.is_empty(){
			continue;
		}
		if state {
			if as_label(&valp){
				sv2.push(valp.to_string());
				state=false;
			}
		}
		else {
			if need_label(valp){
				sv2.push(valp.to_string());
				state=true;
			}
			else if valp.starts_with("}"){
				sv2.push(String::from("ret"));
				sv2.push(valp.to_string());
			}
			else if as_label(&valp) {
				let bb1 = &valp[0..(valp.len()-1)];
				sv2.push(format!("jump {}", bb1));
				sv2.push(valp.to_string());
			}
			else {
				sv2.push(valp.to_string());
			}
		}
	}
	format!("{}{}",predecl, sv2.join("\n"))
}

fn need_label(s:&str)->bool{
	s.starts_with("jump") || s.starts_with("br") || s.starts_with("ret")||s.starts_with("fun")
}
fn as_label(s:&str)->bool{
	s.starts_with("}")||(s.starts_with("%")&& s.ends_with(":"))
}