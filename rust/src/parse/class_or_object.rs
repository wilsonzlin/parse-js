use super::expr::Asi;
use super::pattern::is_valid_pattern_identifier;
use super::pattern::ParsePatternAction;
use super::pattern::ParsePatternRules;
use super::ParseCtx;
use super::Parser;
use crate::ast::ClassMember;
use crate::ast::ClassOrObjectMemberKey;
use crate::ast::ClassOrObjectMemberValue;
use crate::error::SyntaxResult;
use crate::lex::KEYWORDS_MAPPING;
use crate::session::SessionVec;
use crate::source::SourceRange;
use crate::symbol::ScopeType;
use crate::token::TokenType;

pub struct ParseClassBodyResult<'a> {
  pub members: SessionVec<'a, ClassMember<'a>>,
  pub end: SourceRange<'a>,
}

pub struct ParseClassOrObjectMemberResult<'a> {
  pub key: ClassOrObjectMemberKey<'a>,
  pub value: ClassOrObjectMemberValue<'a>,
}

impl<'a> Parser<'a> {
  pub fn parse_class_body(
    &mut self,
    ctx: ParseCtx<'a>,
  ) -> SyntaxResult<'a, ParseClassBodyResult<'a>> {
    self.require(TokenType::BraceOpen)?;
    let mut members = ctx.session.new_vec();
    while self.peek()?.typ() != TokenType::BraceClose {
      // `static` must always come first if present.
      let statik = self.consume_if(TokenType::KeywordStatic)?.is_match();
      let ParseClassOrObjectMemberResult { key, value } = self.parse_class_or_object_member(
        ctx,
        TokenType::Equals,
        TokenType::Semicolon,
        &mut Asi::can(),
      )?;
      self.consume_if(TokenType::Semicolon)?;
      members.push(ClassMember { key, statik, value });
    }
    let end = self.require(TokenType::BraceClose)?.loc_take();
    Ok(ParseClassBodyResult { members, end })
  }

  // It's strictly one of these:
  // <key> [ '=' <expr> ]? [ <asi> | ';' ]
  // async? '*'? <key> '(' ...
  // [ get | set ] <key> '(' ...
  // where <key> = <ident> | <keyword> | <str> | <num> | '[' <expr> ']'
  pub fn parse_class_or_object_member(
    &mut self,
    ctx: ParseCtx<'a>,
    value_delimiter: TokenType,
    statement_delimiter: TokenType,
    property_initialiser_asi: &mut Asi,
  ) -> SyntaxResult<'a, ParseClassOrObjectMemberResult<'a>> {
    let checkpoint = self.checkpoint();
    let mut is_getter = false;
    let mut is_setter = false;
    let mut is_async = false;
    if self.consume_if(TokenType::KeywordGet)?.is_match() {
      is_getter = true;
    } else if self.consume_if(TokenType::KeywordSet)?.is_match() {
      is_setter = true;
    } else if self.consume_if(TokenType::KeywordAsync)?.is_match() {
      is_async = true;
    }
    if is_getter || is_setter || is_async {
      let next_tok = self.peek()?.typ();
      if next_tok == value_delimiter || next_tok == TokenType::ParenthesisOpen {
        // Not actually getter/setter, just using `get`/`set` as property name.
        self.restore_checkpoint(checkpoint);
        is_getter = false;
        is_setter = false;
        is_async = false;
      };
    }
    let is_generator = self.consume_if(TokenType::Asterisk)?.is_match();
    let key = if self.consume_if(TokenType::BracketOpen)?.is_match() {
      let key = ClassOrObjectMemberKey::Computed(self.parse_expr(ctx, TokenType::BracketClose)?);
      self.require(TokenType::BracketClose)?;
      key
    } else {
      let loc = if let Some(str) = self.consume_if(TokenType::LiteralString)?.match_loc_take() {
        // TODO Do we need to remove quotes and/or decode?
        str
      } else if let Some(num) = self.consume_if(TokenType::LiteralNumber)?.match_loc_take() {
        // TODO Do we need to normalise?
        num
      } else if let Some(loc) = self.consume_if(TokenType::PrivateMember)?.match_loc_take() {
        loc
      } else if let Some(loc) = self
        .consume_if_pred(|t| is_valid_pattern_identifier(t.typ(), ctx.rules))?
        .match_loc_take()
      {
        loc
      } else {
        self
          .require_predicate(
            |t| KEYWORDS_MAPPING.contains_key(&t),
            "keyword or identifier",
          )?
          .loc_take()
      };
      ClassOrObjectMemberKey::Direct(loc)
    };
    // Check is_generator/is_async first so that we don't have to check that they're false in every other branch.
    let value = if is_generator || is_async || self.peek()?.typ() == TokenType::ParenthesisOpen {
      let signature = self.parse_signature_function(ctx)?;
      ClassOrObjectMemberValue::Method {
        is_async,
        generator: is_generator,
        signature,
        body: self.parse_stmt_block(ctx.with_rules(ParsePatternRules {
          await_allowed: !is_async && ctx.rules.await_allowed,
          yield_allowed: !is_generator && ctx.rules.yield_allowed,
        }))?,
      }
    } else if is_getter {
      self.require(TokenType::ParenthesisOpen)?;
      self.require(TokenType::ParenthesisClose)?;
      ClassOrObjectMemberValue::Getter {
        body: self.parse_stmt_block(ctx)?,
      }
    } else if is_setter {
      let setter_scope = ctx.create_child_scope(ScopeType::NonArrowFunction);
      let setter_ctx = ctx.with_scope(setter_scope);
      self.require(TokenType::ParenthesisOpen)?;
      let parameter = self.parse_pattern(setter_ctx, ParsePatternAction::AddToClosureScope)?;
      self.require(TokenType::ParenthesisClose)?;
      ClassOrObjectMemberValue::Setter {
        parameter,
        body: self.parse_stmt_block(setter_ctx)?,
      }
    } else if match key {
      ClassOrObjectMemberKey::Direct(_) => match self.peek()? {
        // Given `class A {1}`, `"1" in new A`.
        t if t.typ() == TokenType::BraceClose => true,
        // Given `class A {1;}`, `"1" in new A`.
        t if t.typ() == statement_delimiter => true,
        // Given `class A {1\n2}`, `"2" in new A`.
        t if property_initialiser_asi.can_end_with_asi && t.preceded_by_line_terminator() => true,
        _ => false,
      },
      _ => false,
    } {
      ClassOrObjectMemberValue::Property { initializer: None }
    } else {
      self.require(value_delimiter)?;
      let value = self.parse_expr_until_either_with_asi(
        ctx,
        statement_delimiter,
        TokenType::BraceClose,
        property_initialiser_asi,
      )?;
      ClassOrObjectMemberValue::Property {
        initializer: Some(value),
      }
    };
    Ok(ParseClassOrObjectMemberResult { key, value })
  }
}
