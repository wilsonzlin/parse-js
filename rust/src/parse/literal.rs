use super::pattern::is_valid_pattern_identifier;
use super::ParseCtx;
use super::Parser;
use crate::ast::ClassOrObjectMemberKey;
use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::error::SyntaxResult;
use crate::num::JsNumber;
use crate::session::SessionString;
use crate::source::SourceRange;
use crate::token::TokenType;
use core::str::FromStr;

fn parse_radix(raw: &str, radix: u32) -> Result<f64, ()> {
  u64::from_str_radix(raw, radix)
    .map_err(|_| ())
    // TODO This is lossy, but there is no TryFrom for converting from u64 to f64, and u32 cannot represent all possible JS values.
    .map(|v| v as f64)
}

pub fn normalise_literal_number<'a>(raw: SourceRange<'a>) -> SyntaxResult<'a, JsNumber> {
  // TODO We assume that the Rust parser follows ECMAScript spec and that different representations
  // of the same value get parsed into the same f64 value/bit pattern (e.g. `5.1e10` and `0.51e11`).
  match raw.as_str() {
    s if s.starts_with("0b") || s.starts_with("0B") => parse_radix(&s[2..], 2),
    s if s.starts_with("0o") || s.starts_with("0o") => parse_radix(&s[2..], 8),
    s if s.starts_with("0x") || s.starts_with("0X") => parse_radix(&s[2..], 16),
    s => f64::from_str(s).map_err(|_| ()),
  }
  .map(|n| JsNumber(n))
  .map_err(|_| SyntaxError::from_loc(raw, SyntaxErrorType::MalformedLiteralNumber, None))
}

pub fn normalise_literal_bigint<'a>(
  ctx: ParseCtx<'a>,
  raw: SourceRange<'a>,
) -> SyntaxResult<'a, SessionString<'a>> {
  // TODO Use custom type like JsNumber.
  let mut norm = ctx.session.new_string();
  // TODO
  norm.push_str(raw.as_str());
  Ok(norm)
}

pub fn normalise_literal_string<'a>(
  ctx: ParseCtx<'a>,
  raw: SourceRange<'a>,
) -> SyntaxResult<'a, SessionString<'a>> {
  let mut norm = ctx.session.new_string();
  // TODO Handle escapes.
  norm.push_str(&raw.as_str()[1..raw.len() - 1]);
  Ok(norm)
}

impl<'a> Parser<'a> {
  pub fn parse_and_normalise_literal_string(
    &mut self,
    ctx: ParseCtx<'a>,
  ) -> SyntaxResult<'a, SessionString<'a>> {
    let t = self.require(TokenType::LiteralString)?;
    let s = normalise_literal_string(ctx, t.loc)?;
    Ok(s)
  }

  pub fn parse_class_or_object_member_key(
    &mut self,
    ctx: ParseCtx<'a>,
  ) -> SyntaxResult<'a, ClassOrObjectMemberKey<'a>> {
    Ok(if self.consume_if(TokenType::BracketOpen)?.is_match() {
      let expr = self.parse_expr(ctx, TokenType::BracketClose)?;
      self.require(TokenType::BracketClose)?;
      ClassOrObjectMemberKey::Computed(expr)
    } else {
      let name = self.next()?;
      if !is_valid_pattern_identifier(name.typ, ctx.rules) {
        return Err(name.error(SyntaxErrorType::ExpectedNotFound));
      };
      ClassOrObjectMemberKey::Direct(name.loc)
    })
  }
}
