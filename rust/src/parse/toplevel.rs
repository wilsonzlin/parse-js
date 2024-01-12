use super::pattern::ParsePatternRules;
use super::ParseCtx;
use super::Parser;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxResult;
use crate::token::TokenType;

impl<'a> Parser<'a> {
  pub fn parse_top_level(&mut self) -> SyntaxResult<Node> {
    let ctx = ParseCtx {
      rules: ParsePatternRules {
        await_allowed: true,
        yield_allowed: true,
      },
    };
    let mut body = Vec::new();
    while !self.consume_if(TokenType::EOF)?.is_match() {
      body.push(self.parse_stmt(ctx)?);
    }
    let top_level_node = ctx.create_node(self.source_range(), Syntax::TopLevel { body });
    Ok(top_level_node)
  }
}
