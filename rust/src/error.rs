use crate::loc::Loc;
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
  InvalidLiteralBigInt,
  JsxClosingTagMismatch,
  LineTerminatorAfterArrowFunctionParameters,
  LineTerminatorAfterThrow,
  LineTerminatorAfterYield,
  LineTerminatorInRegex,
  LineTerminatorInString,
  MalformedLiteralNumber,
  RequiredTokenNotFound(TokenType),
  TryStatementHasNoCatchOrFinally,
  UnexpectedEnd,
}

#[derive(Clone)]
pub struct SyntaxError {
  pub typ: SyntaxErrorType,
  pub loc: Loc,
  pub actual_token: Option<TokenType>,
}

impl SyntaxError {
  pub fn new(typ: SyntaxErrorType, loc: Loc, actual_token: Option<TokenType>) -> SyntaxError {
    SyntaxError {
      typ,
      loc,
      actual_token,
    }
  }
}

impl Debug for SyntaxError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{} around loc [{}:{}]", self, self.loc.0, self.loc.1)
  }
}

impl Display for SyntaxError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{:?} [token={:?}]", self.typ, self.actual_token)
  }
}

impl Error for SyntaxError {}

impl PartialEq for SyntaxError {
  fn eq(&self, other: &Self) -> bool {
    self.typ == other.typ
  }
}

impl Eq for SyntaxError {}

pub type SyntaxResult<T> = Result<T, SyntaxError>;
