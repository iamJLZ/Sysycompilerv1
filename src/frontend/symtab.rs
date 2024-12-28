use std::{collections::HashMap, usize};

pub enum NType {
	Varn,
	Funcn,
	Typen,
}

#[derive(Debug, Clone)]
pub enum Type {
	Rtype(usize),
	List(Box<Type>, Vec<usize>),
	Pointer(Box<Type>),
}

#[derive(Debug)]
pub struct TypeEntry {
	pub name: String,
	_comp: String,
}

pub type TypeSymtab = Vec<TypeEntry>;

#[derive(Debug, Clone)]
pub struct VarEntry {
	pub typ: Type,
	pub name: String,
	pub ival: Option<i32>,
}

pub type VarSymtab = Vec<VarEntry>;

#[allow(unused_variables)]
#[derive(Debug)]
pub struct FuncEntry {
	pub name: String,
	pub rettype: Type,
	pub params: Vec<VarEntry>,
	_builtin: bool,
}

impl FuncEntry {
	pub fn new(vname: &str, vrettype: Type, vparams: Vec<VarEntry>, vbuiltin: bool) -> Self {
		FuncEntry {
			name: vname.to_string(),
			rettype: vrettype,
			params: vparams,
			_builtin: vbuiltin,
		}
	}
}

pub type FuncSymtab = Vec<FuncEntry>;

#[derive(Debug)]
pub enum CEType {
	UKI, // unknown ident
	TE,  // type error
}

#[derive(Debug)]
pub struct GVar {
	index: usize,
	funcmap: Vec<HashMap<String, usize>>,
	varmap: Vec<HashMap<String, usize>>,
	pub functab: FuncSymtab,
	pub vartab: VarSymtab,
	pub typetab: TypeSymtab,
	pub loopb: Vec<(usize, usize)>,
}

impl GVar {
	pub fn new() -> Self {
		GVar {
			index: 0,
			funcmap: vec![HashMap::from([
				(String::from("getint"), 0),
				(String::from("getch"), 1),
				(String::from("getarray"), 2),
				(String::from("putint"), 3),
				(String::from("putch"), 4),
				(String::from("putarray"), 5),
				(String::from("starttime"), 6),
				(String::from("stoptime"), 7),
			])],
			varmap: vec![HashMap::new()],
			functab: vec![
				FuncEntry {
					name: String::from("getint"),
					rettype: Type::Rtype(1),
					params: vec![],
					_builtin: true,
				},
				FuncEntry {
					name: String::from("getch"),
					rettype: Type::Rtype(1),
					params: vec![],
					_builtin: true,
				},
				FuncEntry {
					name: String::from("getarray"),
					rettype: Type::Rtype(1),
					params: vec![VarEntry {
						typ: Type::Pointer(Box::new(Type::Rtype(1))),
						name: String::from("p1"),
						ival: None,
					}],
					_builtin: true,
				},
				FuncEntry {
					name: String::from("putint"),
					rettype: Type::Rtype(0),
					params: vec![VarEntry {
						typ: Type::Rtype(1),
						name: String::from("p1"),
						ival: None,
					}],
					_builtin: true,
				},
				FuncEntry {
					name: String::from("putch"),
					rettype: Type::Rtype(0),
					params: vec![VarEntry {
						typ: Type::Rtype(1),
						name: String::from("p1"),
						ival: None,
					}],
					_builtin: true,
				},
				FuncEntry {
					name: String::from("putarray"),
					rettype: Type::Rtype(0),
					params: vec![
						VarEntry {
							typ: Type::Rtype(1),
							name: String::from("p1"),
							ival: None,
						},
						VarEntry {
							typ: Type::Pointer(Box::new(Type::Rtype(1))),
							name: String::from("p2"),
							ival: None,
						},
					],
					_builtin: true,
				},
				FuncEntry {
					name: String::from("starttime"),
					rettype: Type::Rtype(0),
					params: vec![],
					_builtin: true,
				},
				FuncEntry {
					name: String::from("stoptime"),
					rettype: Type::Rtype(0),
					params: vec![],
					_builtin: true,
				},
			],
			vartab: vec![],
			typetab: vec![
				TypeEntry {
					name: String::from("()"),
					_comp: String::new(),
				},
				TypeEntry {
					name: String::from("i32"),
					_comp: String::new(),
				},
			],
			loopb : vec![],
		}
	}
	pub fn reset_index(&mut self) -> () {
		self.index = 0;
	}
	pub fn enter(&mut self) -> () {
		self.funcmap.push(HashMap::new());
		self.varmap.push(HashMap::new());
	}
	pub fn exit(&mut self) -> () {
		self.funcmap.pop();
		self.varmap.pop();
	}
	pub fn require_index(&mut self) -> usize {
		self.index += 1;
		return self.index - 1;
	}
	pub fn register_func(&mut self, entry: FuncEntry) -> usize {
		let midx = self.funcmap.len()-2;
		self.funcmap[midx].insert(entry.name.clone(), self.functab.len().try_into().unwrap());
		self.functab.push(entry);
		self.functab.len() - 1
	}
	pub fn register_var(&mut self, rawname: &str, tp: Type, val: Option<i32>) -> usize {
		let idx = self.require_index();
		let pname = rawname.to_string() + "_" + idx.to_string().as_str();
		self.varmap
			.last_mut()
			.unwrap()
			.insert(rawname.to_string(), self.vartab.len().try_into().unwrap());
		self.vartab.push(VarEntry {
			typ: tp,
			name: pname,
			ival: val,
		});
		self.vartab.len() - 1
	}
	pub fn lookup(&self, name: String, nt: NType) -> Result<usize, String> {
		match nt {
			NType::Funcn => {
				for vi in self.funcmap.iter().rev() {
					match vi.get(&name) {
						Some(a) => return Ok(*a),
						None => continue,
					}
				}
				return Err(String::from("unknown function:") + &name);
			}
			NType::Varn => {
				for vi in self.varmap.iter().rev() {
					match vi.get(&name) {
						Some(a) => return Ok(*a),
						None => continue,
					}
				}
				return Err(String::from("unknown variable:") + &name);
			}
			NType::Typen => Err(String::from("unknown type:") + &name),
		}
	}
	pub fn getval(&self, index: usize) -> VarEntry {
		self.vartab[index].clone()
	}
	pub fn getvarval(&self, index: usize) -> Option<i32> {
		self.vartab[index].ival
	}
	pub fn getfunc(&self, index:usize) -> &FuncEntry{
		&self.functab[index]
	}
	pub fn enter_loop(&mut self){
		self.loopb.push((self.index, self.index+1));
	}
	pub fn level(&self)->usize{
		self.varmap.len()
	}
}
