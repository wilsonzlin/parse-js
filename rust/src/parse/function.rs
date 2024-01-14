use super::ParseCtx;
use super::Parser;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxResult;
use crate::token::TokenType;

impl<'a> Parser<'a> {
  // `scope` should be a newly created closure scope for this function.
  pub fn parse_function_parameters(&mut self, ctx: ParseCtx) -> SyntaxResult<Vec<Node>> {
    let mut parameters = Vec::new();
    self.require(TokenType::ParenthesisOpen)?;
    loop {
      if self.consume_if(TokenType::ParenthesisClose)?.is_match() {
        break;
      };

      let rest = self.consume_if(TokenType::DotDotDot)?.is_match();
      let pattern = self.parse_pattern(ctx)?;
      let default_value = self.consume_if(TokenType::Equals)?.and_then(|| {
        self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::ParenthesisClose)
      })?;

      // TODO Location
      parameters.push(Node::new(pattern.loc, Syntax::ParamDecl {
        rest,
        pattern,
        default_value,
      }));

      if !self.consume_if(TokenType::Comma)?.is_match() {
        self.require(TokenType::ParenthesisClose)?;
        break;
      };
    }
    Ok(parameters)
  }

  pub fn parse_function_body(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::BraceOpen)?;
    let body = self.parse_stmts(ctx, TokenType::BraceClose)?;
    let end = self.require(TokenType::BraceClose)?;
    Ok(Node::new(start.loc + end.loc, Syntax::FunctionBody {
      body,
    }))
  }
}
