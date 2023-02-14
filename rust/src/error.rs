use crate::source::SourceRange;
use crate::token::TokenType;
use core::fmt;
use core::fmt::Debug;
use core::fmt::Formatter;
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
  InvalidCharacterEscape,
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
  pub source: SourceRange<'a>,
  pub typ: SyntaxErrorType,
  pub actual_token: Option<TokenType>,
}

impl<'a> SyntaxError<'a> {
  pub fn new(
    typ: SyntaxErrorType,
    source: SourceRange<'a>,
    actual_token: Option<TokenType>,
  ) -> SyntaxError<'a> {
    SyntaxError {
      typ,
      source,
      actual_token,
    }
  }

  pub fn from_loc(
    loc: SourceRange<'a>,
    typ: SyntaxErrorType,
    actual_token: Option<TokenType>,
  ) -> SyntaxError<'a> {
    SyntaxError {
      source: loc,
      typ,
      actual_token,
    }
  }
}

impl<'a> Debug for SyntaxError<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} around ```{}```", self, self.source.as_str())
  }
}

impl<'a> Display for SyntaxError<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{:?} [token={:?}]", self.typ, self.actual_token,)
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
