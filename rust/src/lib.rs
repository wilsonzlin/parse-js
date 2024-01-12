use ast::Node;
use error::SyntaxResult;
use lex::Lexer;
use parse::toplevel::TopLevelMode;
use parse::Parser;
use session::Session;
use symbol::SymbolGenerator;

pub mod ast;
pub mod char;
pub mod error;
pub mod flag;
pub mod lex;
pub mod num;
pub mod operator;
pub mod parse;
pub mod session;
pub mod source;
pub mod symbol;
pub mod token;
pub mod util;
pub mod visit;

pub fn parse<'a>(
  session: &'a Session,
  source: &'a [u8],
  top_level_mode: TopLevelMode,
) -> SyntaxResult<'a, Node<'a>> {
  let lexer = Lexer::new(source);
  let mut parser = Parser::new(lexer);
  let symbol_generator = SymbolGenerator::new();
  parser.parse_top_level(session, symbol_generator, top_level_mode)
}
