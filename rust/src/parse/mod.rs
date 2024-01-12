use self::pattern::ParsePatternRules;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::error::SyntaxResult;
use crate::lex::lex_next;
use crate::lex::LexMode;
use crate::lex::Lexer;
use crate::lex::LexerCheckpoint;
use crate::loc::Loc;
use crate::token::Token;
use crate::token::TokenType;

pub mod class_or_object;
pub mod decl;
pub mod expr;
pub mod literal;
pub mod operator;
pub mod pattern;
pub mod signature;
pub mod stmt;
#[cfg(test)]
mod tests;
pub mod toplevel;

// Almost every parse_* function takes these field values as parameters. Instead of having to enumerate them as parameters on every function and ordered unnamed arguments on every call, we simply pass this struct around. Fields are public to allow destructuring, but the value should be immutable; the with_* methods can be used to create an altered copy for passing into other functions, which is useful as most calls simply pass through the values unchanged. This struct should be received as a value, not a reference (i.e. `ctx: ParseCtx` not `ctx: &ParseCtx`) as the latter will require a separate lifetime.
// All fields except `session` can (although not often) change between calls, so we don't simply put them in Parser, as otherwise we'd have to "unwind" (i.e. reset) those values after each call returns.
#[derive(Clone, Copy)]
pub struct ParseCtx {
  pub rules: ParsePatternRules, // For simplicity, this is a copy, not a non-mutable reference, to avoid having a separate lifetime for it. The value is only two booleans, so a reference is probably slower, and it's supposed to be immutable (i.e. changes come from altered copying, not mutating the original single instance), so there shouldn't be any difference between a reference and a copy.
}

impl ParseCtx {
  pub fn with_rules(&self, rules: ParsePatternRules) -> ParseCtx {
    ParseCtx { rules, ..*self }
  }

  pub fn create_node(&self, loc: Loc, stx: Syntax) -> Node {
    Node {
      loc,
      stx: Box::new(stx),
    }
  }
}

#[derive(Debug)]
#[must_use]
pub struct MaybeToken {
  typ: TokenType,
  loc: Loc,
  matched: bool,
}

impl MaybeToken {
  pub fn is_match(&self) -> bool {
    self.matched
  }

  pub fn match_loc(&self) -> Option<Loc> {
    if self.matched {
      Some(self.loc)
    } else {
      None
    }
  }

  pub fn error(&self, err: SyntaxErrorType) -> SyntaxError {
    debug_assert!(!self.matched);
    self.loc.error(err, Some(self.typ))
  }

  pub fn and_then<R, F: FnOnce() -> SyntaxResult<R>>(self, f: F) -> SyntaxResult<Option<R>> {
    Ok(if self.matched { Some(f()?) } else { None })
  }
}

pub struct ParserCheckpoint {
  checkpoint: LexerCheckpoint,
}

struct BufferedToken {
  token: Token,
  lex_mode: LexMode,
  after_checkpoint: LexerCheckpoint,
}

pub struct Parser<'a> {
  lexer: Lexer<'a>,
  buffered: Option<BufferedToken>,
}

// We extend this struct with added methods in the various submodules, instead of simply using free functions and passing `&mut Parser` around, for several reasons:
// - Avoid needing to redeclare `<'a>` on every function.
// - More lifetime elision is available for `self` than if it was just another reference parameter.
// - `self` is shorter than `parser` but makes more sense than `p`.
// - Don't need to import each function.
// - Autocomplete is more specific since `self.*` narrows down the options instead of just listing all visible functions (although almost every function currently starts with `parse_` so this is not as significant).
// - For general consistency; if there's no reason why it should be a free function (e.g. more than one ambiguous base type), it should be a method.
// - Makes free functions truly separate independent utility functions.
impl<'a> Parser<'a> {
  pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
    Parser {
      lexer,
      buffered: None,
    }
  }

  pub fn lexer_mut(&mut self) -> &mut Lexer<'a> {
    &mut self.lexer
  }

  pub fn source_range(&self) -> Loc {
    self.lexer.source_range()
  }

  pub fn bytes(&self, loc: Loc) -> &[u8] {
    &self.lexer[loc]
  }

  pub fn str(&self, loc: Loc) -> &str {
    unsafe { std::str::from_utf8_unchecked(self.bytes(loc)) }
  }

  pub fn string(&self, loc: Loc) -> String {
    self.str(loc).to_string()
  }

  pub fn checkpoint(&self) -> ParserCheckpoint {
    ParserCheckpoint {
      checkpoint: self.lexer.checkpoint(),
    }
  }

  pub fn since_checkpoint(&self, checkpoint: ParserCheckpoint) -> Loc {
    self.lexer.since_checkpoint(checkpoint.checkpoint)
  }

  pub fn restore_checkpoint(&mut self, checkpoint: ParserCheckpoint) {
    self.buffered = None;
    self.lexer.apply_checkpoint(checkpoint.checkpoint);
  }

  // Useful if lexer was altered outside parser.
  pub fn clear_buffered(&mut self) {
    self.buffered = None;
  }

  fn forward<K: FnOnce(&Token) -> bool>(
    &mut self,
    mode: LexMode,
    keep: K,
  ) -> SyntaxResult<(bool, Token)> {
    match self.buffered.as_ref() {
      Some(b) if b.lex_mode == mode => Ok(if keep(&b.token) {
        self.lexer.apply_checkpoint(b.after_checkpoint);
        (true, self.buffered.take().unwrap().token)
      } else {
        (false, b.token.clone())
      }),
      _ => {
        // Don't use self.checkpoint as self.backtrack will clear buffer.
        let cp = self.lexer.checkpoint();
        let t = lex_next(&mut self.lexer, mode)?;
        let k = keep(&t);
        self.buffered = if k {
          None
        } else {
          let after_checkpoint = self.lexer.checkpoint();
          self.lexer.apply_checkpoint(cp);
          Some(BufferedToken {
            token: t.clone(),
            lex_mode: mode,
            after_checkpoint,
          })
        };
        Ok((k, t))
      }
    }
  }

  pub fn next_with_mode(&mut self, mode: LexMode) -> SyntaxResult<Token> {
    self.forward(mode, |_| true).map(|r| r.1)
  }

  pub fn next(&mut self) -> SyntaxResult<Token> {
    self.next_with_mode(LexMode::Standard)
  }

  pub fn peek_with_mode(&mut self, mode: LexMode) -> SyntaxResult<Token> {
    self.forward(mode, |_| false).map(|r| r.1)
  }

  pub fn peek(&mut self) -> SyntaxResult<Token> {
    self.peek_with_mode(LexMode::Standard)
  }

  pub fn consume_peeked(&mut self) -> Token {
    let b = self.buffered.take().unwrap();
    self.lexer.apply_checkpoint(b.after_checkpoint);
    b.token
  }

  pub fn maybe_with_mode(&mut self, typ: TokenType, mode: LexMode) -> SyntaxResult<MaybeToken> {
    let (matched, t) = self.forward(mode, |t| t.typ == typ)?;
    Ok(MaybeToken {
      typ,
      matched,
      loc: t.loc,
    })
  }

  pub fn consume_if(&mut self, typ: TokenType) -> SyntaxResult<MaybeToken> {
    self.maybe_with_mode(typ, LexMode::Standard)
  }

  pub fn consume_if_pred<F: FnOnce(&Token) -> bool>(
    &mut self,
    pred: F,
  ) -> SyntaxResult<MaybeToken> {
    let (matched, t) = self.forward(LexMode::Standard, pred)?;
    Ok(MaybeToken {
      typ: t.typ,
      matched,
      loc: t.loc,
    })
  }

  pub fn require_with_mode(&mut self, typ: TokenType, mode: LexMode) -> SyntaxResult<Token> {
    let t = self.next_with_mode(mode)?;
    if t.typ != typ {
      Err(t.error(SyntaxErrorType::RequiredTokenNotFound(typ)))
    } else {
      Ok(t)
    }
  }

  pub fn require_predicate<P: FnOnce(TokenType) -> bool>(
    &mut self,
    pred: P,
    expected: &'static str,
  ) -> SyntaxResult<Token> {
    let t = self.next_with_mode(LexMode::Standard)?;
    if !pred(t.typ) {
      Err(t.error(SyntaxErrorType::ExpectedSyntax(expected)))
    } else {
      Ok(t)
    }
  }

  pub fn require(&mut self, typ: TokenType) -> SyntaxResult<Token> {
    self.require_with_mode(typ, LexMode::Standard)
  }
}
