use std::vec;

pub type Comp = Vec<Decl>;

#[derive(Debug)]
pub enum Decl {
	FuncDef(FuncDef),
	VarsDec(VarInit),
}

#[derive(Debug)]
pub enum VarInit {
	Constvar,
	VarDef(usize, Box<Expr>),
	ListDef(usize, FlatList),
}
#[derive(Debug)]
pub enum ListInit {
	Expred(Box<Expr>),
	Listed(Vec<ListInit>),
}

#[derive(Debug,Clone)]
pub enum FlatListInit{
	Nonzero(Vec<i32>),
	Zeros(usize),
	Notnum(Vec<Expr>),
}

#[derive(Debug,Clone)]
pub struct FlatList(pub Vec<FlatListInit>, pub usize);

fn if_align(dims:&[usize], cntc:usize)->usize{
	if cntc == 0{
		return dims.len()-1;
	}
	let mut cnt = cntc;
	let mut res:usize = 0;
	for d in dims.iter().rev(){
		if (cnt%d)==0 {
			res+=1;
			cnt/=d;
		}
		else {
			break;
		}
	}
	res
}


impl FlatList {
	pub fn new()->Self{
		FlatList(
			vec![],
			0,
		)
	}
	pub fn zero_new(n:usize)->Self{
		FlatList(vec![FlatListInit::Zeros(n)], n)
	}
	fn append(&mut self, other:&mut Self){
		let mut it = other.0.iter_mut();
		match (self.0.last_mut(), it.next()) {
			(Some(a),Some(b)) => {
				match (a,b) {
					(FlatListInit::Zeros(m),FlatListInit::Zeros(n)) => {
						*n+=*m;
						self.0.pop();
					},
					(FlatListInit::Nonzero(m),FlatListInit::Nonzero(n)) => {
						n.append(m);
						self.0.pop();
					},
					(FlatListInit::Notnum(m),FlatListInit::Notnum(n)) => {
					   n.append(m);
					   self.0.pop();
					},
					(_,_)=>{},
				};
				self.0.append(&mut other.0);
			}
			_ => self.0.append(&mut other.0),
		};
		self.1+=other.1;
		()
	}
	fn push(&mut self, mut element:FlatListInit){
		match (self.0.last_mut(),&mut element){
			(Some(FlatListInit::Nonzero(a)),FlatListInit::Nonzero(b))=>{
				a.append(b);
			},
			(Some(FlatListInit::Notnum(a)),FlatListInit::Notnum(b))=>{
				a.append(b);
			},
			(Some(FlatListInit::Zeros(a)),FlatListInit::Zeros(b))=>{
				*a+=*b;
			}
			_ =>{
				self.0.push(element);
			}
		}
		self.1+=1;
	}
	fn zero_fill(&mut self, size:usize)->Result<(),String>{
		match self.0.last_mut() {
			Some(FlatListInit::Zeros(a)) => *a +=size,
			_ => self.0.push(FlatListInit::Zeros(size)),
		}
		self.1+=size;
		Ok(())
	}
	pub fn from_listinit(initer:Vec<ListInit>,dims:&[usize])->Result<FlatList, String>{

		let mut res = FlatList(vec![],0);
		for init in initer{
			match init {
				ListInit::Expred(ex) => {
					match *ex {
						Expr::Num(0) => res.push(FlatListInit::Zeros(1)),
						Expr::Num(a) if a!=0 => res.push(FlatListInit::Nonzero(vec![a])),
						_ => res.push(FlatListInit::Notnum(vec![*ex])),
					}
				},
				ListInit::Listed(lt) => {
					let alg = if_align(dims, res.1);
					if alg==0{
						return Err(String::from("Expect a scalar, but found an aggregate value\n"));
					}
					let mut rh = FlatList::from_listinit(lt, &dims[(dims.len()-alg)..])?;
					res.append(&mut rh);
				}
			}
		}
		let capa :usize= dims.iter().product();
		if res.1<capa{
			res.zero_fill(capa-res.1)?;
		}
		Ok(res)
	}
}

#[derive(Debug)]
pub struct FuncDef {
	pub id: usize,
	pub body: Box<Block>,
}

pub type Block = Vec<BlockItem>;

#[derive(Debug)]
pub enum BlockItem {
	Stmt(Stmt),
	Decl(Decl),
}

#[derive(Debug)]
pub enum Stmt {
	Expr(Box<Expr>),
	Block(Box<Block>),
	Break,
	Continue,
	Return(Option<Box<Expr>>),
	Branc {
		cond: Box<Expr>,
		pass: Box<Stmt>,
		fail: Option<Box<Stmt>>,
	},
	Loop(Box<Expr>, Box<Stmt>),
	NONE,
}

#[derive(Debug, Clone)]
pub enum Expr {
	Num(i32),
	Var(usize),
	FuncCall(usize, Vec<Expr>),
	IndexCall(usize, Vec<Expr>),

	Asg(Box<Expr>, Box<Expr>),
	Addasg(Box<Expr>, Box<Expr>),
	Subasg(Box<Expr>, Box<Expr>),
	Mulasg(Box<Expr>, Box<Expr>),
	Divasg(Box<Expr>, Box<Expr>),
	Modasg(Box<Expr>, Box<Expr>),
	Shlasg(Box<Expr>, Box<Expr>),
	Shrasg(Box<Expr>, Box<Expr>),
	Xorasg(Box<Expr>, Box<Expr>),
	Bitandasg(Box<Expr>, Box<Expr>),
	Bitorasg(Box<Expr>, Box<Expr>),
	Logand(Box<Expr>, Box<Expr>),
	Logor(Box<Expr>, Box<Expr>),
	Bitor(Box<Expr>, Box<Expr>),
	Xor(Box<Expr>, Box<Expr>),
	Bitand(Box<Expr>, Box<Expr>),
	Neq(Box<Expr>, Box<Expr>),
	Eq(Box<Expr>, Box<Expr>),
	Leq(Box<Expr>, Box<Expr>),
	Geq(Box<Expr>, Box<Expr>),
	Les(Box<Expr>, Box<Expr>),
	Grt(Box<Expr>, Box<Expr>),
	Sar(Box<Expr>, Box<Expr>),
	Shr(Box<Expr>, Box<Expr>),
	Shl(Box<Expr>, Box<Expr>),
	Sub(Box<Expr>, Box<Expr>),
	Add(Box<Expr>, Box<Expr>),
	Moo(Box<Expr>, Box<Expr>),
	Mul(Box<Expr>, Box<Expr>),
	Div(Box<Expr>, Box<Expr>),
	Neg(Box<Expr>),
	Pos(Box<Expr>),
	Not(Box<Expr>),
	Bitnot(Box<Expr>),
}
