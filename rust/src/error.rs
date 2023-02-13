use crate::source::SourceRange;
use crate::token::TokenType;
use core::cmp::max;
use core::cmp::min;
use core::fmt;
use core::fmt::Debug;
use core::fmt::Formatter;
use core::str::from_utf8_unchecked;
use std::error::Error;
use std::fmt::Display;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SyntaxErrorType {
  ExpectedNotFound,
  ExpectedSyntax(&'static str),
  ForLoopHeaderHasInvalidLhs,
  ForLoopHeaderHasMultipleDeclarators,
  ForLoopHeaderHasNoLhs,
  InvalidAssigmentTarget,
  LineTerminatorAfterArrowFunctionParameters,
  LineTerminatorAfterThrow,
  LineTerminatorAfterYield,
  LineTerminatorInRegex,
  LineTerminatorInString,
  MalformedLiteralNumber,
  JsxClosingTagMismatch,
  RequiredTokenNotFound(TokenType),
  TryStatementHasNoCatchOrFinally,
  UnexpectedEnd,
}

#[derive(Clone)]
pub struct SyntaxError<'a> {
  pub source: &'a [u8],
  pub position: usize,
  pub typ: SyntaxErrorType,
  pub actual_token: Option<TokenType>,
}

impl<'a> SyntaxError<'a> {
  pub fn new(
    typ: SyntaxErrorType,
    source: &'a [u8],
    position: usize,
    actual_token: Option<TokenType>,
  ) -> SyntaxError<'a> {
    SyntaxError {
      typ,
      source,
      position,
      actual_token,
    }
  }

  pub fn from_loc(
    loc: SourceRange<'a>,
    typ: SyntaxErrorType,
    actual_token: Option<TokenType>,
  ) -> SyntaxError<'a> {
    SyntaxError {
      source: loc.source,
      typ,
      position: loc.start,
      actual_token,
    }
  }
}

impl<'a> Debug for SyntaxError<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} around ```{}```", self, unsafe {
      from_utf8_unchecked(
        &self.source[max(0, self.position as isize - 40) as usize
          ..min(self.source.len() as isize, self.position as isize + 40) as usize],
      )
    })
  }
}

impl<'a> Display for SyntaxError<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{:?} [position={} token={:?}]",
      self.typ, self.position, self.actual_token,
    )
  }
}

impl<'a> Error for SyntaxError<'a> {}

impl<'a> PartialEq for SyntaxError<'a> {
  fn eq(&self, other: &Self) -> bool {
    self.typ == other.typ
  }
}

impl<'a> Eq for SyntaxError<'a> {}

pub type SyntaxResult<'a, T> = Result<T, SyntaxError<'a>>;
