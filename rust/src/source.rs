use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::token::TokenType;
use core::cmp::max;
use core::cmp::min;
use core::cmp::Eq;
use core::fmt;
use core::fmt::Debug;
use core::fmt::Formatter;
use core::hash::Hash;
use core::hash::Hasher;
use core::ops::Add;
use core::ops::AddAssign;
use std::fmt::Display;
use std::marker::PhantomData;

/// A string backed by a source. Treated as a string, so contents rather than position is considered the value (and this is used for equality). However, it's illegal to add two SourceRange values from different sources.
// We want to make this Copy, as it's also very frequently used. Having to use `&` (esp. with operators) and wrestling with reference lifetimes (esp. with methods) everywhere without necessity probably makes using this by Copy more worthwhile on the whole. This parser will probably be used for visiting and mutating, so `.clone()` littered everywhere by library users also hinders readability. SourceRange is essentially like a pointer/reference/slice anyway, and those are always treated like primitive Copy values.
// However, because we use it a lot, if we want Copy, we need to make it as small as possible. Therefore, we degrade the reference to a const pointer so we can use smaller u32 offsets. We still want to keep the original pointer (and not simply offset it by `start`) so we can assert safety when adding two SourceRange values (i.e. sources are identical).
// We used to store the original source as a reference, an "edit" override `&'a [u8]` (to allow edits while still retaining original source and position for debuggability), and offsets as u64; however, this ended up being too big, and copying it around became very costly. We now drop the "edit" override, as it should be inferrable from the tree where the original position was, and Node values should retain their original `loc` even if their `stx` has updated SourceRange (e.g. edited identifiers). Using an "edit" also made it unwiedly to use, even for basic equality and hashing, as we needed an original SourceRange and then add an edit.
#[derive(Copy, Clone)]
pub struct SourceRange<'a> {
  phantom: PhantomData<&'a [u8]>,
  source: *const u8,
  start: u32,
  end: u32,
}

impl<'a> SourceRange<'a> {
  pub fn new(source: &'a [u8], start: usize, end: usize) -> SourceRange<'a> {
    debug_assert!(start <= end);
    debug_assert!(end <= source.len());
    SourceRange {
      phantom: PhantomData,
      source: source.as_ptr(),
      start: start.try_into().unwrap(),
      end: end.try_into().unwrap(),
    }
  }

  pub fn from_slice(source: &'a [u8]) -> SourceRange<'a> {
    SourceRange::new(source, 0, source.len())
  }

  pub fn start(&self) -> usize {
    self.start.try_into().unwrap()
  }

  pub fn end(&self) -> usize {
    self.end.try_into().unwrap()
  }

  pub fn at_start(&self) -> SourceRange<'a> {
    SourceRange {
      phantom: PhantomData,
      source: self.source,
      start: self.start,
      end: self.start,
    }
  }

  pub fn at_end(&self) -> SourceRange<'a> {
    SourceRange {
      phantom: PhantomData,
      source: self.source,
      start: self.end,
      end: self.end,
    }
  }

  pub fn error(self, typ: SyntaxErrorType, actual_token: Option<TokenType>) -> SyntaxError<'a> {
    SyntaxError::from_loc(self, typ, actual_token)
  }

  #[inline(always)]
  pub fn add_option(self, rhs: Option<SourceRange<'a>>) -> SourceRange<'a> {
    let mut new = self;
    if let Some(rhs) = rhs {
      new.extend(rhs);
    };
    new
  }

  #[inline(always)]
  pub fn is_empty(&self) -> bool {
    self.end == self.start
  }

  #[inline(always)]
  pub fn as_slice(&self) -> &[u8] {
    unsafe {
      // TODO SAFETY: Does this work if EOF?
      core::slice::from_raw_parts(self.source.add(self.start()), self.len())
    }
  }

  #[inline(always)]
  pub fn as_str(&self) -> &str {
    unsafe { core::str::from_utf8_unchecked(self.as_slice()) }
  }

  #[inline(always)]
  pub fn len(&self) -> usize {
    (self.end - self.start).try_into().unwrap()
  }

  #[inline(always)]
  pub fn extend(&mut self, other: SourceRange) {
    assert_eq!(self.source, other.source);
    self.start = min(self.start, other.start);
    self.end = max(self.end, other.end);
  }
}

impl<'a> Add for SourceRange<'a> {
  type Output = SourceRange<'a>;

  #[inline(always)]
  fn add(self, rhs: Self) -> Self::Output {
    let mut new = self;
    new.extend(rhs);
    new
  }
}

impl<'a> AddAssign for SourceRange<'a> {
  #[inline(always)]
  fn add_assign(&mut self, rhs: Self) {
    self.extend(rhs);
  }
}

impl<'a> Debug for SourceRange<'a> {
  #[inline(always)]
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "`{}`[{}:{}]", self.as_str(), self.start, self.end)
  }
}

impl<'a> Display for SourceRange<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    f.write_str(self.as_str())
  }
}

impl<'a> Eq for SourceRange<'a> {}

impl<'a> Hash for SourceRange<'a> {
  #[inline(always)]
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.as_slice().hash(state);
  }
}

impl<'a> PartialEq for SourceRange<'a> {
  #[inline(always)]
  fn eq(&self, other: &Self) -> bool {
    self.as_slice() == other.as_slice()
  }
}

impl<'a> PartialEq<&[u8]> for SourceRange<'a> {
  #[inline(always)]
  fn eq(&self, other: &&[u8]) -> bool {
    &self.as_slice() == other
  }
}

impl<'a> PartialEq<&str> for SourceRange<'a> {
  #[inline(always)]
  fn eq(&self, other: &&str) -> bool {
    &self.as_str() == other
  }
}

#[cfg(feature = "serialize")]
impl<'a> serde::Serialize for SourceRange<'a> {
  fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(self.as_str())
  }
}
