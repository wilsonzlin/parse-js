use super::pattern::ParsePatternAction;
use super::ParseCtx;
use super::Parser;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxResult;
use crate::token::TokenType;

impl<'a> Parser<'a> {
  // `scope` should be a newly created closure scope for this function.
  pub fn parse_signature_function(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let start_pos = self.checkpoint();

    let mut parameters = ctx.session.new_vec();
    self.require(TokenType::ParenthesisOpen)?;
    loop {
      if self.consume_if(TokenType::ParenthesisClose)?.is_match() {
        break;
      };

      let rest = self.consume_if(TokenType::DotDotDot)?.is_match();
      let pattern = self.parse_pattern(ctx, ParsePatternAction::AddToClosureScope)?;
      let default_value = self.consume_if(TokenType::Equals)?.and_then(|| {
        self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::ParenthesisClose)
      })?;

      // TODO Location
      parameters.push(ctx.create_node(pattern.loc().clone(), Syntax::ParamDecl {
        rest,
        pattern,
        default_value,
      }));

      if !self.consume_if(TokenType::Comma)?.is_match() {
        self.require(TokenType::ParenthesisClose)?;
        break;
      };
    }

    Ok(ctx.create_node(
      self.since_checkpoint(start_pos),
      Syntax::FunctionSignature { parameters },
    ))
  }
}
