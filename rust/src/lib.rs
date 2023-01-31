use ast::NodeId;
use ast::NodeMap;
use error::SyntaxResult;
use lex::Lexer;
use parse::parser::Parser;
use parse::toplevel::parse_top_level;
use parse::toplevel::ParseTopLevelResult;
use parse::toplevel::TopLevelMode;
use symbol::ScopeId;
use symbol::ScopeMap;

pub mod ast;
pub mod char;
pub mod error;
pub mod lex;
pub mod num;
pub mod operator;
pub mod parse;
#[cfg(feature = "serialize")]
pub mod serialize;
pub mod source;
pub mod symbol;
pub mod token;
pub mod update;
pub mod util;
pub mod visit;

pub struct ParseOutput {
  pub node_map: NodeMap,
  pub scope_map: ScopeMap,
  pub top_level_node_id: NodeId,
  pub top_level_scope_id: ScopeId,
}

pub fn parse(source: Vec<u8>, top_level_mode: TopLevelMode) -> SyntaxResult<ParseOutput> {
  let lexer = Lexer::new(source);
  let mut parser = Parser::new(lexer);
  let ParseTopLevelResult {
    top_level_node_id,
    top_level_scope_id,
  } = parse_top_level(&mut parser, top_level_mode)?;
  let (node_map, scope_map) = parser.take();
  Ok(ParseOutput {
    node_map,
    scope_map,
    top_level_node_id,
    top_level_scope_id,
  })
}
