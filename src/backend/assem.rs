use std::collections::HashMap;
use std::fmt::Display;

use koopa::ir::entities::ValueData;
use koopa::ir::{self, BasicBlock, BinaryOp, TypeKind, Value, ValueKind};

use super::directive::Directive;
use super::inst::Ins;
use super::reg::Reg;

static CALL_REG:[Reg;8]=[
	Reg::A0,
	Reg::A1,
	Reg::A2,
	Reg::A3,
	Reg::A4,
	Reg::A5,
	Reg::A6,
	Reg::A7,
];

#[derive(Debug)]
pub enum AssemItem {
	Label(String),
	Inst(Ins),
	Directive(Directive),
	Comment(String),
}

pub struct Assem(pub Vec<AssemItem>);

impl Assem {
	pub fn new()->Self{
		Self(vec![])
	}
	pub fn add_label(&mut self, label:String){
		self.0.push(AssemItem::Label(label));
	}
	pub fn add_inst(&mut self, ins:Ins){
		self.0.push(AssemItem::Inst(ins));
	}
	pub fn add_directive(&mut self, direc:Directive){
		self.0.push(AssemItem::Directive(direc));
	}
	pub fn add_comment(&mut self, com:String){
		self.0.push(AssemItem::Comment(com));
	}
}

impl Display for AssemItem {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let s = match self {
			AssemItem::Label(label) => format!("{}:", label),
			AssemItem::Inst(ins) => ins.to_string(),
			AssemItem::Directive(direc) => direc.to_string(),
			AssemItem::Comment(comment) => format!("#{comment}"),
		};
		write!(f, "{}", s)
	}
}

impl Display for Assem {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}\n", self.0.iter().map(AssemItem::to_string).collect::<Vec<String>>().join("\n"))
	}
}

pub struct ProgInfo<'a>{
	v2offset: HashMap<Value, i32>,
	gv2label: HashMap<Value, String>,
	frame_size:i32,
	bb2label:HashMap<BasicBlock, String>,
	pub prog:&'a ir::Program,
	pub assembly:Assem,
	func:ir::Function,
	r_size:i32,
	a_size:i32,
}

impl<'a> ProgInfo<'a>{
	pub fn new(program:&'a ir::Program, function:ir::Function)->Self{
		ProgInfo { v2offset: HashMap::new(), gv2label:HashMap::new(), frame_size: 0, bb2label: HashMap::new(), prog:& program, assembly:Assem::new(), func:function, r_size:0, a_size:0}
	}
	fn switch_func(&mut self, function:ir::Function){
		self.func=function;
		let funcd = self.prog.func(self.func);
		if funcd.layout().entry_bb().is_none(){
			return;
		}
		self.assembly.add_directive(Directive::Text);
		self.assembly.add_directive(Directive::Globl((funcd.name()[1..]).to_string()));
		self.assembly.add_label((funcd.name()[1..]).to_string());
		let calls = self.prog.func(self.func).dfg().values().iter().filter_map(|(_,vd)|->Option<i32>{
			match vd.kind() {
				ValueKind::Call(cl) =>{
					Some(cl.args().len() as i32)
				},
				_ => None,
			}
		}).collect::<Vec<_>>();
		self.r_size = if calls.is_empty() {0} else {4};
		self.a_size = match calls.iter().max(){
			Some(a) => if *a>8 { a*4-32 } else {0},
			None => 0,
		};
		let mut s_size = self.a_size;
		self.v2offset.clear();
		for (_, bbn) in self.prog.func(self.func).layout().bbs(){
			for v in bbn.insts().keys(){
				self.v2offset.insert(*v, s_size);
				match self.get_value(v).kind() {
					ValueKind::Alloc(_) =>{
						match self.get_value(v).ty().kind() {
							TypeKind::Pointer(tpt) => {
								s_size+=tpt.size() as i32;
							},
							_ => unreachable!(),
						}
					},
					_ =>{
						s_size+=self.get_value(v).ty().size() as i32;
					},
				}
			}
		}
		self.frame_size = (s_size+self.r_size + 15)&!15;
		// prologue
		self.assembly.add_inst(Ins::Addi(Reg::Sp, Reg::Sp, -self.frame_size));
		if self.r_size!=0{
			self.assembly.add_inst(Ins::Sw(Reg::Ra, Reg::Sp, self.frame_size-4));
		}

		for (bb ,_) in funcd.layout().bbs(){
			self.bb2label.insert(*bb, 
				funcd.dfg().bb(*bb).name().clone().unwrap()[1..].to_string()
			);
		}

		for (bb, bbn) in funcd.layout().bbs(){
			let label = self.bb2label.get(bb).unwrap().clone();
			self.assembly.add_label(label);
			for vi in bbn.insts().keys(){
				self.assembly.add_comment(match self.prog.func(self.func).dfg().value(vi.clone()).name(){
					Some(nam) => nam.clone(),
					None => format!("{:?}", self.prog.func(self.func).dfg().value(vi.clone()).kind())
				});
				self.generate_func_value(vi);
			}
		}
	}
	pub fn generate(program:ir::Program)->Assem{
		let funcs = program.func_layout()[0];
		let mut pg = ProgInfo::new(&program, funcs);
		// skip global value now

		for (va,vd) in pg.prog.borrow_values().iter(){
			if let ValueKind::GlobalAlloc(gba)=vd.kind(){
				let name = vd.name().clone().unwrap()[1..].to_string();
				pg.assembly.add_directive(Directive::Data);
				pg.assembly.add_directive(Directive::Globl(name.clone()));
				pg.assembly.add_label(name.clone());
				let init = gba.init();
				match pg.prog.borrow_value(init).kind() {
					ValueKind::ZeroInit(_)=>{
						if let TypeKind::Pointer(pt) = vd.ty().kind(){
							pg.assembly.add_directive(Directive::Zero(pt.size()));
							pg.gv2label.insert(*va, name);
						}
					},
					ValueKind::Aggregate(aggre) =>{
						let eles = aggre.elems().iter().map(|v|->i32{
							if let ValueKind::Integer(t) = pg.prog.borrow_value(*v).kind(){
								t.value()
							}
							else {
								unreachable!()
							}
						})
						.collect::<Vec<_>>();
						pg.assembly.add_directive(Directive::Word(eles));
						pg.gv2label.insert(*va, name);
					},
					ValueKind::Integer(t)=>{
						pg.assembly.add_directive(Directive::Word(vec![t.value()]));
						pg.gv2label.insert(*va, name);
					},
					_ => unreachable!(),
				}
			}
		}

		// functions
		for (fc, _) in program.funcs().iter(){
			pg.switch_func(*fc);
		}
		pg.assembly
	}
	fn get_offset(&self, value:&Value)->i32{
		*self.v2offset.get(value).unwrap()
	}
	fn get_value(&self, value:&Value)->ValueData{
		self.prog.func(self.func).dfg().value(*value).clone()
	}
	fn generate_func_value(&mut self, value:&Value){
		let vd = self.get_value(value);
		match vd.kind() {
			ValueKind::Binary(b) => {
				let lhs = b.lhs();
				let rhs = b.rhs();
				let mut rs1 = Reg::T0;
				let mut rs2 = Reg::T1;
				let rd = Reg::T2;
				self.load(&lhs, &mut rs1);
				self.load(&rhs, &mut rs2);
				match b.op() {
					BinaryOp::Add => self.assembly.add_inst(Ins::Add(rd, rs1, rs2)),
					BinaryOp::Div => self.assembly.add_inst(Ins::Div(rd, rs1, rs2)),
					BinaryOp::And => self.assembly.add_inst(Ins::And(rd, rs1, rs2)),
					BinaryOp::Eq => {
						self.assembly.add_inst(Ins::Xor(rd, rs1, rs2));
						self.assembly.add_inst(Ins::Seqz(rd, rd));
					},
					BinaryOp::Ge => {
						self.assembly.add_inst(Ins::Slt(rd, rs1, rs2));
						self.assembly.add_inst(Ins::Seqz(rd, rd));
					},
					BinaryOp::Gt => self.assembly.add_inst(Ins::Sgt(rd, rs1, rs2)),
					BinaryOp::Le => {
						self.assembly.add_inst(Ins::Sgt(rd, rs1, rs2));
						self.assembly.add_inst(Ins::Seqz(rd, rd));
					},
					BinaryOp::Lt => self.assembly.add_inst(Ins::Slt(rd, rs1, rs2)),
					BinaryOp::Mod => self.assembly.add_inst(Ins::Rem(rd, rs1, rs2)),
					BinaryOp::Mul => self.assembly.add_inst(Ins::Mul(rd, rs1, rs2)),
					BinaryOp::NotEq => {
						self.assembly.add_inst(Ins::Xor(rd, rs1, rs2));
						self.assembly.add_inst(Ins::Snez(rd, rd));
					},
					BinaryOp::Or => self.assembly.add_inst(Ins::Or(rd, rs1, rs2)),
					BinaryOp::Sar => self.assembly.add_inst(Ins::Sra(rd, rs1, rs2)),
					BinaryOp::Shl => self.assembly.add_inst(Ins::Sll(rd, rs1, rs2)),
					BinaryOp::Shr => self.assembly.add_inst(Ins::Srl(rd, rs1, rs2)),
					BinaryOp::Sub => self.assembly.add_inst(Ins::Sub(rd, rs1, rs2)),
					BinaryOp::Xor => self.assembly.add_inst(Ins::Xor(rd, rs1, rs2)),
				}
				self.save(value, rd);
			},
			ValueKind::Return(rt) => {
				match rt.value() {
					Some(v) => {
						let mut rs = Reg::A0;
						self.load(&v, &mut rs);
						if rs!=Reg::A0{
							self.assembly.add_inst(Ins::Mv(Reg::A0, rs));
						}
					}
					_ => {},
				}
				if self.r_size!=0{
					self.assembly.add_inst(Ins::Lw(Reg::Ra, Reg::Sp, self.frame_size-4));
				}
				self.assembly.add_inst(Ins::Addi(Reg::Sp, Reg::Sp, self.frame_size));
				self.assembly.add_inst(Ins::Ret);
			},
			ValueKind::Alloc(_) =>{},
			ValueKind::Load(ld)=>{
				let rd = Reg::T2;
				let src = ld.src();
				if let Some(lab) = self.gv2label.get(&src){
					self.assembly.add_inst(Ins::La(rd, lab.clone()));
					self.assembly.add_inst(Ins::Lw(rd, rd, 0));
				}
				else if let ValueKind::GetElemPtr(_) = self.prog.func(self.func).dfg().value(src).kind().clone() {
					let offset = self.get_offset(&src);
					self.assembly.add_inst(Ins::Lw(Reg::T0, Reg::Sp, offset));
					self.assembly.add_inst(Ins::Lw(rd, Reg::T0, 0));
				}
				else if let ValueKind::GetPtr(_) = self.prog.func(self.func).dfg().value(src).kind().clone() {
					let offset = self.get_offset(&src);
					self.assembly.add_inst(Ins::Lw(Reg::T0, Reg::Sp, offset));
					self.assembly.add_inst(Ins::Lw(rd, Reg::T0, 0));
				}
				else {
					let offset = self.get_offset(&src);
					self.assembly.add_inst(Ins::Lw(rd, Reg::Sp, offset));
				}
				self.save(value, rd);
			},
			ValueKind::Branch(brc) => {
				let t_label = self.bb2label.get(&brc.true_bb()).unwrap().clone();
				let f_label = self.bb2label.get(&brc.false_bb()).unwrap().clone();
				let mut rd = Reg::T0;
				self.load(&brc.cond(), &mut rd);
				self.assembly.add_inst(Ins::Bnez(rd, t_label));
				self.assembly.add_inst(Ins::J(f_label));
			},
			ValueKind::Store(st)=>{
				let dst = st.dest();
				let val = st.value();
				let mut rs2 = Reg::T1;
				self.load(&val, &mut rs2);
				if let Some(lab) = self.gv2label.get(&dst){
					let rd = Reg::T2;
					self.assembly.add_inst(Ins::La(rd, lab.clone()));
					self.assembly.add_inst(Ins::Sw(rs2, rd, 0));
				}
				else if let ValueKind::GetElemPtr(_) = self.prog.func(self.func).dfg().value(dst).kind().clone() {
					let offset = self.get_offset(&dst);
					self.assembly.add_inst(Ins::Lw(Reg::T0, Reg::Sp, offset));
					self.assembly.add_inst(Ins::Sw(rs2, Reg::T0, 0));
				}
				else if let ValueKind::GetPtr(_) = self.prog.func(self.func).dfg().value(dst).kind().clone() {
					let offset = self.get_offset(&dst);
					self.assembly.add_inst(Ins::Lw(Reg::T0, Reg::Sp, offset));
					self.assembly.add_inst(Ins::Sw(rs2, Reg::T0, 0));
				}
				else {
					let offset = self.get_offset(&dst);
					self.assembly.add_inst(Ins::Sw(rs2, Reg::Sp, offset));
				}
			},
			ValueKind::Jump(jump) => {
				self.assembly.add_inst(Ins::J(self.bb2label.get(&jump.target()).unwrap().clone()))
			},
			ValueKind::Call(cal) => {
				let args = cal.args().iter();
				for (i,arg) in args.enumerate(){
					if i<8{
						let mut rd = CALL_REG[i];
						self.load(arg, &mut rd);
						if rd!=CALL_REG[i]{
							self.assembly.add_inst(Ins::Add(CALL_REG[i], Reg::Zero, rd));
						}
					}
					else {
						let mut rd = Reg::T0;
						self.load(arg, &mut rd);
						self.assembly.add_inst(Ins::Sw(rd, Reg::Sp, (i-8) as i32 * 4));
					}
				}
				let funcname = self.prog.func(cal.callee()).name()[1..].to_string();
				self.assembly.add_inst(Ins::Call(funcname));
				if self.prog.func(self.func).dfg().value(*value).ty().is_i32(){
					self.save(value, Reg::A0);
				}
			},
			ValueKind::GetElemPtr(gep)=>{
				let src = gep.src();
				let index = gep.index();
				let bsize :i32= if let TypeKind::Pointer(_) = self.val_type(&src).kind(){
					4	// i32 only now
				}
				else{
					unreachable!();
				};
				let src_reg = Reg::T0;
				let mut index_reg  =Reg::T1;
				match self.gv2label.get(&src) {
					Some(n)=>{
						self.assembly.add_inst(Ins::La(src_reg, n.clone()));
					},
					None =>{
						self.assembly.add_inst(Ins::Addi(src_reg, Reg::Sp, *self.v2offset.get(&src).unwrap()));
					}
				}
				self.load(&index, &mut index_reg);
				if index_reg!=Reg::Zero{
					if bsize&(bsize-1)==0{
						self.assembly.add_inst(Ins::Slli(index_reg, index_reg, bsize.trailing_zeros() as i32));
					}
					else {
						self.assembly.add_inst(Ins::Li(Reg::T2, bsize as i32));
						self.assembly.add_inst(Ins::Mul(index_reg, index_reg, Reg::T2));
					}
					self.assembly.add_inst(Ins::Add(src_reg, src_reg, index_reg))
				}
				self.save(value, src_reg);
			},
			ValueKind::GetPtr(gep)=>{
				let src = gep.src();
				let index = gep.index();
				let bsize = if let TypeKind::Pointer(base) = self.val_type(&src).kind(){
					base.size()
				}
				else{
					unreachable!();
				};
				let src_reg = Reg::T0;
				let mut index_reg  =Reg::T1;
				self.assembly.add_inst(Ins::Lw(src_reg, Reg::Sp, *self.v2offset.get(&src).unwrap()));
				self.load(&index, &mut index_reg);
				if index_reg!=Reg::Zero{
					if bsize&(bsize-1)==0{
						self.assembly.add_inst(Ins::Slli(index_reg, index_reg, bsize.trailing_zeros() as i32));
					}
					else {
						self.assembly.add_inst(Ins::Li(Reg::T2, bsize as i32));
						self.assembly.add_inst(Ins::Mul(index_reg, index_reg, Reg::T2));
					}
					self.assembly.add_inst(Ins::Add(src_reg, src_reg, index_reg))
				}
				self.save(value, src_reg);
			}
			_ =>{},
		}
	}
	fn load(&mut self, value:&Value, reg:&mut Reg){
		match self.get_value(value).kind() {
			ValueKind::Integer(itg)=>{
				let itgv = itg.value();
				if itgv==0{
					*reg=Reg::Zero;
				}
				else {
					self.assembly.add_inst(Ins::Li(*reg, itgv));
				}
			},
			ValueKind::FuncArgRef(fa)=>{
				let i = fa.index();
				if i<8 {
					*reg = CALL_REG[i];
				}
				else {
					self.assembly.add_inst(Ins::Lw(*reg, Reg::Sp, self.frame_size+(i-8) as i32 *4));
				}
			},
			_ =>{
				self.assembly.add_inst(Ins::Lw(*reg, Reg::Sp,* self.v2offset.get(value).unwrap()));
			}
		}
	}
	fn save(&mut self, value:&Value, reg:Reg){
		self.assembly.add_inst(Ins::Sw(reg, Reg::Sp,* self.v2offset.get(value).unwrap()))
	}
	fn val_type(&self, value:&Value)->koopa::ir::Type{
		match self.prog.func(self.func).dfg().values().get(value) {
			Some(v) => v.ty().clone(),
			None => self.prog.borrow_value(*value).ty().clone(),
		}
	}
}