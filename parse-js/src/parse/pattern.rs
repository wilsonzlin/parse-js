use super::ParseCtx;
use super::Parser;
use crate::ast::ArrayPatternElement;
use crate::ast::ClassOrObjectMemberKey;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxErrorType;
use crate::error::SyntaxResult;
use crate::token::TokenType;
use crate::token::UNRESERVED_KEYWORDS;

#[derive(Clone, Copy)]
pub struct ParsePatternRules {
  // `await` is not allowed as an arrow function parameter or a parameter/variable inside an async function.
  pub await_allowed: bool,
  // `yield` is not allowed as a parameter/variable inside a generator function.
  pub yield_allowed: bool,
}

impl ParsePatternRules {
  pub fn with_await_allowed(&self, await_allowed: bool) -> ParsePatternRules {
    Self {
      await_allowed,
      ..*self
    }
  }

  pub fn with_yield_allowed(&self, yield_allowed: bool) -> ParsePatternRules {
    Self {
      yield_allowed,
      ..*self
    }
  }
}

pub fn is_valid_pattern_identifier(typ: TokenType, rules: ParsePatternRules) -> bool {
  match typ {
    TokenType::Identifier => true,
    TokenType::KeywordAwait => rules.await_allowed,
    TokenType::KeywordYield => rules.yield_allowed,
    t => UNRESERVED_KEYWORDS.contains(&t),
  }
}

impl<'a> Parser<'a> {
  fn parse_pattern_identifier(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    if !is_valid_pattern_identifier(self.peek()?.typ, ctx.rules) {
      return Err(
        self
          .peek()?
          .error(SyntaxErrorType::ExpectedSyntax("identifier")),
      );
    }
    let t = self.next()?;
    let node = Node::new(t.loc, Syntax::IdentifierPattern {
      name: self.string(t.loc),
    });
    Ok(node)
  }

  pub fn parse_pattern(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let checkpoint = self.checkpoint();
    let t = self.next()?;
    Ok(match t.typ {
      t if is_valid_pattern_identifier(t, ctx.rules) => {
        self.restore_checkpoint(checkpoint);
        self.parse_pattern_identifier(ctx)?
      }
      TokenType::BraceOpen => {
        let mut properties = Vec::new();
        let mut rest = None;
        loop {
          if self.peek()?.typ == TokenType::BraceClose {
            break;
          };
          let mut loc = self.peek()?.loc;
          // Check inside loop to ensure that it must come first or after a comma.
          if self.consume_if(TokenType::DotDotDot)?.is_match() {
            rest = Some(self.parse_pattern_identifier(ctx)?);
            break;
          };

          let (key_loc, key) = if self.consume_if(TokenType::BracketOpen)?.is_match() {
            let expr = self.parse_expr(ctx, TokenType::BracketClose)?;
            self.require(TokenType::BracketClose)?;
            (expr.loc, ClassOrObjectMemberKey::Computed(expr))
          } else {
            let name = self.next()?;
            if !is_valid_pattern_identifier(name.typ, ctx.rules) {
              return Err(name.error(SyntaxErrorType::ExpectedNotFound));
            };
            (
              name.loc,
              ClassOrObjectMemberKey::Direct(self.string(name.loc)),
            )
          };
          let (shorthand, target) = if self.consume_if(TokenType::Colon)?.is_match() {
            (false, self.parse_pattern(ctx)?)
          } else {
            match &key {
              ClassOrObjectMemberKey::Computed(name) => {
                return Err(name.error(SyntaxErrorType::ExpectedSyntax(
                  "object pattern property subpattern",
                )));
              }
              ClassOrObjectMemberKey::Direct(name) => (
                true,
                Node::new(key_loc, Syntax::IdentifierPattern { name: name.clone() }),
              ),
            }
          };
          let default_value = if self.consume_if(TokenType::Equals)?.is_match() {
            Some(self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::BraceClose)?)
          } else {
            None
          };
          loc.extend(default_value.as_ref().unwrap_or(&target).loc);
          let direct_key_name = match &key {
            ClassOrObjectMemberKey::Direct(name) => Some(name.clone()),
            _ => None,
          };
          let property = Node::new(loc, Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
            shorthand,
          });
          properties.push(property);
          // This will break if `}`.
          if !self.consume_if(TokenType::Comma)?.is_match() {
            break;
          };
        }
        let close = self.require(TokenType::BraceClose)?;
        Node::new(t.loc + close.loc, Syntax::ObjectPattern {
          properties,
          rest,
        })
      }
      TokenType::BracketOpen => {
        let mut elements = Vec::<Option<ArrayPatternElement>>::new();
        let mut rest = None;
        loop {
          if self.consume_if(TokenType::BracketClose)?.is_match() {
            break;
          };
          // Check inside loop to ensure that it must come first or after a comma.
          if self.consume_if(TokenType::DotDotDot)?.is_match() {
            rest = Some(self.parse_pattern(ctx)?);
            break;
          };

          // An unnamed element is allowed to ignore that element.
          if self.consume_if(TokenType::Comma)?.is_match() {
            elements.push(None);
          } else {
            let target = self.parse_pattern(ctx)?;
            let default_value = if self.consume_if(TokenType::Equals)?.is_match() {
              Some(self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::BracketClose)?)
            } else {
              None
            };
            elements.push(Some(ArrayPatternElement {
              target,
              default_value,
            }));
            // This will break if `]`.
            if !self.consume_if(TokenType::Comma)?.is_match() {
              break;
            };
          };
        }
        let close = self.require(TokenType::BracketClose)?;
        Node::new(t.loc + close.loc, Syntax::ArrayPattern { elements, rest })
      }
      _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("pattern"))),
    })
  }
}
