use super::expr::Asi;
use super::pattern::is_valid_pattern_identifier;
use super::pattern::ParsePatternRules;
use super::ParseCtx;
use super::Parser;
use crate::ast::ClassOrObjectMemberKey;
use crate::ast::ClassOrObjectMemberValue;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxResult;
use crate::lex::KEYWORDS_MAPPING;
use crate::loc::Loc;
use crate::token::TokenType;

pub struct ParseClassBodyResult {
  pub members: Vec<Node>, // Always ClassMember.
  pub end: Loc,
}

pub struct ParseClassOrObjectMemberResult {
  pub key: ClassOrObjectMemberKey,
  pub key_loc: Loc,
  pub value: ClassOrObjectMemberValue,
  pub value_loc: Loc,
}

impl<'a> Parser<'a> {
  pub fn parse_class_body(&mut self, ctx: ParseCtx) -> SyntaxResult<ParseClassBodyResult> {
    self.require(TokenType::BraceOpen)?;
    let mut members = Vec::new();
    while self.peek()?.typ != TokenType::BraceClose {
      // `static` must always come first if present.
      let static_ = self.consume_if(TokenType::KeywordStatic)?.match_loc();
      let ParseClassOrObjectMemberResult {
        key,
        key_loc,
        value,
        value_loc,
      } = self.parse_class_or_object_member(
        ctx,
        TokenType::Equals,
        TokenType::Semicolon,
        &mut Asi::can(),
      )?;
      self.consume_if(TokenType::Semicolon)?;
      members.push(Node::new(
        key_loc.add_option(static_) + value_loc,
        Syntax::ClassMember {
          key,
          static_: static_.is_some(),
          value,
        },
      ));
    }
    let end = self.require(TokenType::BraceClose)?.loc;
    Ok(ParseClassBodyResult { members, end })
  }

  // It's strictly one of these:
  // <key> [ '=' <expr> ]? [ <asi> | ';' ]
  // async? '*'? <key> '(' ...
  // [ get | set ] <key> '(' ...
  // where <key> = <ident> | <keyword> | <str> | <num> | '[' <expr> ']'
  pub fn parse_class_or_object_member(
    &mut self,
    ctx: ParseCtx,
    value_delimiter: TokenType,
    statement_delimiter: TokenType,
    property_initialiser_asi: &mut Asi,
  ) -> SyntaxResult<ParseClassOrObjectMemberResult> {
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
      let next_tok = self.peek()?.typ;
      if next_tok == value_delimiter || next_tok == TokenType::ParenthesisOpen {
        // Not actually getter/setter, just using `get`/`set` as property name.
        self.restore_checkpoint(checkpoint);
        is_getter = false;
        is_setter = false;
        is_async = false;
      };
    }
    let is_generator = self.consume_if(TokenType::Asterisk)?.is_match();
    let (key_loc, key) = if self.consume_if(TokenType::BracketOpen)?.is_match() {
      let key = self.parse_expr(ctx, TokenType::BracketClose)?;
      self.require(TokenType::BracketClose)?;
      (key.loc, ClassOrObjectMemberKey::Computed(key))
    } else {
      let loc = if let Some(str) = self.consume_if(TokenType::LiteralString)?.match_loc() {
        // TODO Do we need to remove quotes and/or decode?
        str
      } else if let Some(num) = self.consume_if(TokenType::LiteralNumber)?.match_loc() {
        // TODO Do we need to normalise?
        num
      } else if let Some(loc) = self.consume_if(TokenType::PrivateMember)?.match_loc() {
        loc
      } else if let Some(loc) = self
        .consume_if_pred(|t| is_valid_pattern_identifier(t.typ, ctx.rules))?
        .match_loc()
      {
        loc
      } else {
        self
          .require_predicate(
            |t| KEYWORDS_MAPPING.contains_key(&t),
            "keyword or identifier",
          )?
          .loc
      };
      (loc, ClassOrObjectMemberKey::Direct(self.string(loc)))
    };
    // Check is_generator/is_async first so that we don't have to check that they're false in every other branch.
    let (value_loc, value) =
      if is_generator || is_async || self.peek()?.typ == TokenType::ParenthesisOpen {
        let parameters = self.parse_function_parameters(ctx)?;
        let body = self.parse_function_body(ctx.with_rules(ParsePatternRules {
          await_allowed: !is_async && ctx.rules.await_allowed,
          yield_allowed: !is_generator && ctx.rules.yield_allowed,
        }))?;
        (body.loc, ClassOrObjectMemberValue::Method {
          function: Node::new(body.loc, Syntax::Function {
            async_: is_async,
            generator: is_generator,
            parameters,
            body,
          }),
        })
      } else if is_getter {
        let mut loc = self.require(TokenType::ParenthesisOpen)?.loc;
        self.require(TokenType::ParenthesisClose)?;
        let body = self.parse_function_body(ctx)?;
        loc += body.loc;
        (loc, ClassOrObjectMemberValue::Getter {
          function: Node::new(loc, Syntax::Function {
            async_: false,
            body,
            generator: false,
            parameters: Vec::new(),
          }),
        })
      } else if is_setter {
        let mut loc = self.require(TokenType::ParenthesisOpen)?.loc;
        let param = self.parse_pattern(ctx)?;
        self.require(TokenType::ParenthesisClose)?;
        let body = self.parse_function_body(ctx)?;
        loc += body.loc;
        (loc, ClassOrObjectMemberValue::Setter {
          function: Node::new(loc, Syntax::Function {
            async_: false,
            generator: false,
            body,
            parameters: vec![Node::new(param.loc, Syntax::ParamDecl {
              rest: false,
              pattern: param,
              default_value: None,
            })],
          }),
        })
      } else if match key {
        ClassOrObjectMemberKey::Direct(_) => match self.peek()? {
          // Given `class A {1}`, `"1" in new A`.
          t if t.typ == TokenType::BraceClose => true,
          // Given `class A {1;}`, `"1" in new A`.
          t if t.typ == statement_delimiter => true,
          // Given `class A {1\n2}`, `"2" in new A`.
          t if property_initialiser_asi.can_end_with_asi && t.preceded_by_line_terminator => true,
          _ => false,
        },
        _ => false,
      } {
        (key_loc, ClassOrObjectMemberValue::Property {
          initializer: None,
        })
      } else {
        let loc_start = self.require(value_delimiter)?.loc;
        let value = self.parse_expr_until_either_with_asi(
          ctx,
          statement_delimiter,
          TokenType::BraceClose,
          property_initialiser_asi,
        )?;
        (loc_start + value.loc, ClassOrObjectMemberValue::Property {
          initializer: Some(value),
        })
      };
    Ok(ParseClassOrObjectMemberResult {
      key,
      key_loc,
      value,
      value_loc,
    })
  }
}
