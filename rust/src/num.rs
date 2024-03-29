use core::hash::Hash;
use core::hash::Hasher;
use core::mem;
use serde::Serialize;
use serde::Serializer;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

// This provides Eq for f64.
#[derive(Copy, Clone, PartialOrd, Debug)]
pub struct JsNumber(pub f64);

impl Display for JsNumber {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl PartialEq for JsNumber {
  fn eq(&self, other: &Self) -> bool {
    if self.0.is_nan() {
      return other.0.is_nan();
    };
    self.0.eq(&other.0)
  }
}

impl Eq for JsNumber {}

impl Ord for JsNumber {
  fn cmp(&self, other: &Self) -> Ordering {
    // Only NaNs cannot be compared, and we treat them as equal.
    self.0.partial_cmp(&other.0).unwrap_or(Ordering::Equal)
  }
}

impl Hash for JsNumber {
  fn hash<H: Hasher>(&self, state: &mut H) {
    if !self.0.is_nan() {
      unsafe { mem::transmute::<f64, u64>(self.0) }.hash(state);
    };
  }
}

impl Serialize for JsNumber {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_f64(self.0)
  }
}
