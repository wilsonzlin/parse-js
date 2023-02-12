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

/// A string backed by a source. Treated as a string, so contents rather than position is considered the value. It's illegal to operate on two SourceRange values from different sources, like Add or Eq. Adding two SourceRange (Add, AddAssign, add_option, etc.) values is illegal if at least one has an edit, due to ambiguity. Performing these illegal actions may result in a panic.
// It's debatable whether to make this Copy. It's a bit large, but it's also very frequently used. Having to use `&` (esp. with operators) and wrestling with reference lifetimes (esp. with methods) everywhere without necessity probably makes using this by Copy more worthwhile on the whole. This parser will probably be used for visiting and mutating, so `.clone()` littered everywhere by library users also hinders readability. The compiler most likely can "see through" copying that is simply using or changing one field and perform relevant optimisations. SourceRange is essentially like a pointer/reference/slice anyway, and those are always treated like primitive Copy values.
#[derive(Copy, Clone)]
pub struct SourceRange<'a> {
  pub source: &'a [u8],
  // This replaces the previous Source::Anonymous functionality. Instead of having SourceRange values point to different Source backings, including anonymous ones that own their own Vec<u8> for edits to the input code, and then replacing the original SourceRange, we now use an optional overlay over the original SourceRange. This allows preserving the original location and context that the edit is supposed to replace, useful for debugging and error message/context formatting. To insert, use `start == end`; to delete, use `edit == Some(b"")`.
  // The edit value should be immutable inside the arena. The idea here is to create a SessionVec, build the edit, and then copy the bytes into the Session as a slice (to make ownership escape Vec), so we can use a reference instead of an owned Vec. This allows for cheap copying and sharing and no possibility of mutation.
  pub edit: Option<&'a [u8]>,
  pub start: usize,
  pub end: usize,
}

#[cfg(feature = "serialize")]
impl<'a> serde::Serialize for SourceRange<'a> {
  fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(self.as_str())
  }
}

impl<'a> SourceRange<'a> {
  #[inline(always)]
  fn debug_assert_same_source(&self, other: SourceRange<'a>) {
    debug_assert!(core::ptr::eq(self.source, other.source));
  }

  #[inline(always)]
  fn debug_assert_can_merge(&self, other: Option<SourceRange<'a>>) {
    debug_assert!(self.edit.is_none());
    if let Some(other) = other {
      debug_assert!(other.edit.is_none());
      self.debug_assert_same_source(other);
    };
  }

  #[inline(always)]
  pub fn get_start_of_source(&self) -> SourceRange<'a> {
    SourceRange {
      source: self.source,
      edit: None,
      start: 0,
      end: 0,
    }
  }

  #[inline(always)]
  pub fn get_end_of_source(&self) -> SourceRange<'a> {
    SourceRange {
      source: self.source,
      edit: None,
      start: self.source.len(),
      end: self.source.len(),
    }
  }

  #[inline(always)]
  pub fn add_option(&self, rhs: Option<SourceRange<'a>>) -> SourceRange<'a> {
    self.debug_assert_can_merge(rhs);
    SourceRange {
      source: self.source,
      edit: None,
      start: min(self.start, rhs.map(|l| l.start).unwrap_or(0)),
      end: max(self.end, rhs.map(|l| l.end).unwrap_or(0)),
    }
  }

  #[inline(always)]
  pub fn with_edit(&self, edit: &'a [u8]) -> SourceRange<'a> {
    SourceRange {
      edit: Some(edit),
      ..*self
    }
  }

  #[inline(always)]
  pub fn is_empty(&self) -> bool {
    self.end == self.start
  }

  #[inline(always)]
  pub fn is_eof(&self) -> bool {
    self.start >= self.source.len()
  }

  #[inline(always)]
  pub fn as_slice(&self) -> &[u8] {
    self
      .edit
      .unwrap_or_else(|| &self.source[self.start..self.end])
  }

  #[inline(always)]
  pub fn as_str(&self) -> &str {
    unsafe { core::str::from_utf8_unchecked(self.as_slice()) }
  }

  #[inline(always)]
  pub fn len(&self) -> usize {
    self.end - self.start
  }

  #[inline(always)]
  pub fn extend(&mut self, other: SourceRange) {
    self.debug_assert_can_merge(Some(other));
    self.start = min(self.start, other.start);
    self.end = max(self.end, other.end);
  }
}

impl<'a> Add for SourceRange<'a> {
  type Output = SourceRange<'a>;

  #[inline(always)]
  fn add(self, rhs: Self) -> Self::Output {
    self.debug_assert_can_merge(Some(rhs));
    SourceRange {
      source: self.source,
      edit: None,
      start: min(self.start, rhs.start),
      end: max(self.end, rhs.end),
    }
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
    if self.is_eof() {
      Ok(())
    } else {
      write!(f, "`{}`[{}:{}]", self.as_str(), self.start, self.end)
    }
  }
}

impl<'a> Eq for SourceRange<'a> {}

impl<'a> Hash for SourceRange<'a> {
  #[inline(always)]
  fn hash<H: Hasher>(&self, state: &mut H) {
    if !self.is_eof() {
      self.as_slice().hash(state);
    };
  }
}

impl<'a> PartialEq for SourceRange<'a> {
  #[inline(always)]
  fn eq(&self, other: &Self) -> bool {
    self.debug_assert_same_source(*other);
    if self.is_eof() {
      other.is_eof()
    } else {
      self.as_slice() == other.as_slice()
    }
  }
}

impl<'a> PartialEq<&str> for SourceRange<'a> {
  #[inline(always)]
  fn eq(&self, other: &&str) -> bool {
    &self.as_str() == other
  }
}
