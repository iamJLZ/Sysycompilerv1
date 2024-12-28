pub mod frontend;
pub mod backend;
use frontend::parse::parse_file;
use std::{env,fs};
use frontend::ir::prog_ir;
use backend::assem::ProgInfo;
fn main() {
	koopa::ir::Type::set_ptr_size(4);
	let args:Vec<String> = env::args().collect();
	let mode = &args[1];
	let inputfile = &args[2];
	let outputfile = &args[4];
	let input = match fs::read_to_string(inputfile.clone()){
		Ok(a) => a,
		Err(_)  => {
			println!("Invalid input path: {}",inputfile);
			return;
		}
	};
	let mut gvar = frontend::symtab::GVar::new();
	let ast = match parse_file(&input,&mut gvar){
		Ok(a) => a,
		Err(b) => {
			println!("{}",b);
			return;
		}
	};
	//fs::write("ast.txt", format!("{ast:#?}")).unwrap();
	let fir = prog_ir(&ast, &mut gvar);
	match mode.as_str() {
		"-koopa" => {
			fs::write(outputfile, fir).unwrap();
		},
		"-riscv" => {
			let program = koopa::front::Driver::from(fir).generate_program().unwrap();
			let assem = ProgInfo::generate(program);
			fs::write(outputfile, assem.to_string()).unwrap();
		},
		"-perf" => unreachable!(),
		_ => unreachable!(),
	}
}
