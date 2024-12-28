#[derive(Debug)]
pub enum Error {
	E0001(String),	// unknown identifer
	E0002(String),	// mismatched type
	E0003(String),	// lack of return value
	E0004(String),	// unused variable
	E0005(String),	// try modifying a constant
}