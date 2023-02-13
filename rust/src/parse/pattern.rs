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

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParsePatternAction {
  None,
  AddToBlockScope,
  // For var statements. Note that this won't add to the top-level if it's a global and that's the closest, since global declarators cannot be minified.
  AddToClosureScope,
}

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
  fn parse_pattern_identifier(
    &mut self,
    ctx: ParseCtx<'a>,
    action: ParsePatternAction,
  ) -> SyntaxResult<'a, Node<'a>> {
    if !is_valid_pattern_identifier(self.peek()?.typ, ctx.rules) {
      return Err(
        self
          .peek()?
          .error(SyntaxErrorType::ExpectedSyntax("identifier")),
      );
    }
    let t = self.next()?;
    let node = ctx.create_node(t.loc, Syntax::IdentifierPattern { name: t.loc });
    match action {
      ParsePatternAction::None => {}
      ParsePatternAction::AddToBlockScope => {
        ctx.scope.add_block_symbol(t.loc)?;
      }
      ParsePatternAction::AddToClosureScope => {
        if let Some(closure) = ctx.scope.find_self_or_ancestor(|t| t.is_closure()) {
          closure.add_symbol(t.loc)?;
        };
      }
    };
    Ok(node)
  }

  pub fn parse_pattern(
    &mut self,
    ctx: ParseCtx<'a>,
    action: ParsePatternAction,
  ) -> SyntaxResult<'a, Node<'a>> {
    let checkpoint = self.checkpoint();
    let t = self.next()?;
    Ok(match t.typ {
      t if is_valid_pattern_identifier(t, ctx.rules) => {
        self.restore_checkpoint(checkpoint);
        self.parse_pattern_identifier(ctx, action)?
      }
      TokenType::BraceOpen => {
        let mut properties = ctx.session.new_vec();
        let mut rest = None;
        loop {
          if self.peek()?.typ == TokenType::BraceClose {
            break;
          };
          let mut loc = self.peek()?.loc;
          // Check inside loop to ensure that it must come first or after a comma.
          if self.consume_if(TokenType::DotDotDot)?.is_match() {
            rest = Some(self.parse_pattern_identifier(ctx, action)?);
            break;
          };

          let key = self.parse_class_or_object_member_key(ctx)?;
          let target = if self.consume_if(TokenType::Colon)?.is_match() {
            Some(self.parse_pattern(ctx, action)?)
          } else {
            if let ClassOrObjectMemberKey::Computed(name) = key {
              return Err(name.error(SyntaxErrorType::ExpectedSyntax(
                "object pattern property subpattern",
              )));
            };
            None
          };
          let default_value = if self.consume_if(TokenType::Equals)?.is_match() {
            Some(self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::BraceClose)?)
          } else {
            None
          };
          if let Some(n) = default_value.as_ref().or(target.as_ref()) {
            loc.extend(n.loc);
          };
          let direct_key_name = match &key {
            ClassOrObjectMemberKey::Direct(name) => Some(name.clone()),
            _ => None,
          };
          let target_exists = target.is_some();
          let property = ctx.create_node(loc, Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
          });
          properties.push(property);
          match (direct_key_name, target_exists, action) {
            (Some(name), false, ParsePatternAction::AddToBlockScope) => {
              ctx.scope.add_block_symbol(name)?;
            }
            (Some(name), false, ParsePatternAction::AddToClosureScope) => {
              if let Some(closure) = ctx.scope.find_self_or_ancestor(|t| t.is_closure()) {
                closure.add_symbol(name)?;
              }
            }
            _ => {}
          };
          // This will break if `}`.
          if !self.consume_if(TokenType::Comma)?.is_match() {
            break;
          };
        }
        let close = self.require(TokenType::BraceClose)?;
        ctx.create_node(t.loc + close.loc, Syntax::ObjectPattern {
          properties,
          rest,
        })
      }
      TokenType::BracketOpen => {
        let mut elements = ctx.session.new_vec::<Option<ArrayPatternElement>>();
        let mut rest = None;
        loop {
          if self.consume_if(TokenType::BracketClose)?.is_match() {
            break;
          };
          // Check inside loop to ensure that it must come first or after a comma.
          if self.consume_if(TokenType::DotDotDot)?.is_match() {
            rest = Some(self.parse_pattern(ctx, action)?);
            break;
          };

          // An unnamed element is allowed to ignore that element.
          if self.consume_if(TokenType::Comma)?.is_match() {
            elements.push(None);
          } else {
            let target = self.parse_pattern(ctx, action)?;
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
        ctx.create_node(t.loc + close.loc, Syntax::ArrayPattern { elements, rest })
      }
      _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("pattern"))),
    })
  }
}
