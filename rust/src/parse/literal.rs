use super::ParseCtx;
use super::Parser;
use crate::error::SyntaxErrorType;
use crate::error::SyntaxResult;
use crate::num::JsNumber;
use crate::token::TokenType;
use core::str::FromStr;
use memchr::memchr;
use std::str::from_utf8_unchecked;

fn parse_radix(raw: &str, radix: u32) -> Result<f64, ()> {
  u64::from_str_radix(raw, radix)
    .map_err(|_| ())
    // TODO This is lossy, but there is no TryFrom for converting from u64 to f64, and u32 cannot represent all possible JS values.
    .map(|v| v as f64)
}

pub fn normalise_literal_number(raw: &str) -> Option<JsNumber> {
  // TODO We assume that the Rust parser follows ECMAScript spec and that different representations
  // of the same value get parsed into the same f64 value/bit pattern (e.g. `5.1e10` and `0.51e11`).
  match raw {
    s if s.starts_with("0b") || s.starts_with("0B") => parse_radix(&s[2..], 2),
    s if s.starts_with("0o") || s.starts_with("0o") => parse_radix(&s[2..], 8),
    s if s.starts_with("0x") || s.starts_with("0X") => parse_radix(&s[2..], 16),
    s => f64::from_str(s).map_err(|_| ()),
  }
  .map(|n| JsNumber(n))
  .ok()
}

pub fn normalise_literal_bigint(ctx: ParseCtx, raw: &str) -> Option<String> {
  // TODO Use custom type like JsNumber.
  // TODO
  Some(raw.to_string())
}

pub fn normalise_literal_string_or_template_inner(ctx: ParseCtx, mut raw: &[u8]) -> Option<String> {
  let mut norm = Vec::new();
  while !raw.is_empty() {
    let Some(escape_pos) = memchr(b'\\', raw) else {
      norm.extend_from_slice(raw);
      break;
    };
    norm.extend_from_slice(&raw[..escape_pos]);
    raw = &raw[escape_pos + 1..];
    // https://mathiasbynens.be/notes/javascript-escapes
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#escape_sequences
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals#tagged_templates_and_escape_sequences
    let mut tmp = [0u8; 4];
    let (skip, add): (usize, &[u8]) = match raw[0] {
      b'\n' => (1, b""),
      b'b' => (1, b"\x08"),
      b'f' => (1, b"\x0c"),
      b'n' => (1, b"\n"),
      b'r' => (1, b"\r"),
      b't' => (1, b"\t"),
      b'v' => (1, b"\x0b"),
      b'0'..=b'7' => {
        // Octal escape.
        let mut len = 1;
        if raw.get(len).filter(|&&c| c >= b'0' && c <= b'7').is_some() {
          len += 1;
          if raw.get(len).filter(|&&c| c >= b'0' && c <= b'7').is_some() {
            len += 1;
          };
        };
        char::from_u32(
          u32::from_str_radix(unsafe { from_utf8_unchecked(&raw[..len]) }, 8).unwrap(),
        )
        .unwrap()
        .encode_utf8(&mut tmp);
        (len, tmp.as_slice())
      }
      b'x' => {
        // Hexadecimal escape.
        if raw.len() < 3 || !raw[1].is_ascii_hexdigit() || !raw[2].is_ascii_hexdigit() {
          return None;
        };
        char::from_u32(
          u32::from_str_radix(unsafe { from_utf8_unchecked(&raw[1..3]) }, 16).unwrap(),
        )
        .unwrap()
        .encode_utf8(&mut tmp);
        (3, tmp.as_slice())
      }
      b'u' => match raw.get(1) {
        Some(b'{') => {
          // Unicode code point escape.
          let Some(end_pos) = memchr(b'}', raw) else {
            return None;
          };
          if end_pos < 3 || end_pos > 8 {
            return None;
          };
          let cp =
            u32::from_str_radix(unsafe { from_utf8_unchecked(&raw[2..end_pos]) }, 16).ok()?;
          let c = char::from_u32(cp)?;
          c.encode_utf8(&mut tmp);
          (end_pos + 1, tmp.as_slice())
        }
        Some(_) => {
          // Unicode escape.
          if raw.len() < 5 {
            return None;
          };
          let cp = u32::from_str_radix(unsafe { from_utf8_unchecked(&raw[1..5]) }, 16).ok()?;
          let c = char::from_u32(cp)?;
          c.encode_utf8(&mut tmp);
          (5, tmp.as_slice())
        }
        None => {
          return None;
        }
      },
      c => (1, {
        tmp[0] = c;
        &tmp[..1]
      }),
    };
    norm.extend_from_slice(add);
    raw = &raw[skip..];
  }
  // We return str instead of [u8] so that serialisation is easy and str methods are available.
  Some(String::from_utf8(norm).unwrap())
}

pub fn normalise_literal_string(ctx: ParseCtx, raw: &str) -> Option<String> {
  normalise_literal_string_or_template_inner(ctx, &raw.as_bytes()[1..raw.len() - 1])
}

impl<'a> Parser<'a> {
  pub fn parse_and_normalise_literal_string(&mut self, ctx: ParseCtx) -> SyntaxResult<String> {
    let t = self.require(TokenType::LiteralString)?;
    normalise_literal_string(ctx, self.str(t.loc))
      .ok_or_else(|| t.loc.error(SyntaxErrorType::InvalidCharacterEscape, None))
  }
}
