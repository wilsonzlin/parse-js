use lazy_static::lazy_static;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::str::from_utf8_unchecked;

// These are separate JS files so that any TypeScript-based IDE will provide nice syntax highlighting and quick verification that the types exist.
const DEFS: &str = include_str!("./builtin.js");
const DEFS_TYPEDARRAY: &str = include_str!("./builtin.TypedArray.js");

// Make the type `&[u8]` instead of `&str` for compatibility with `SourceRange`.
// We don't want to use a `Vec` as that will make it hard to query with `SessionVec<u8>` or `&[&[u8]]`; therefore, we use a fixed length tuple where unused elements are empty strings, as tuples can be allocated on the stack and therefore don't have their own lifetimes (both making them easy to use). It's currently OK as no builtin has a longer path than 3.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Builtin<'a>(pub &'a [u8], pub &'a [u8], pub &'a [u8]);

impl<'a> Display for Builtin<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", unsafe { from_utf8_unchecked(self.0) })?;
    if self.1 != b"" {
      write!(f, "{}", unsafe { from_utf8_unchecked(self.1) })?;
    };
    if self.2 != b"" {
      write!(f, "{}", unsafe { from_utf8_unchecked(self.2) })?;
    };
    Ok(())
  }
}

impl<'a> Debug for Builtin<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self)
  }
}

// https://github.com/rust-lang/rust/issues/89265#issuecomment-927263213
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct BuiltinEntry<'a>(Builtin<'a>);

impl<'a> BuiltinEntry<'a> {
  pub fn get(&self) -> Builtin<'a> {
    self.0
  }
}

impl<'short, 'long: 'short> Borrow<Builtin<'short>> for BuiltinEntry<'long> {
  fn borrow(&self) -> &Builtin<'short> {
    &self.0
  }
}

lazy_static! {
  pub static ref BUILTINS: HashSet<BuiltinEntry<'static>> = {
    let typedarray_defs = DEFS_TYPEDARRAY
      .split("\n")
      .filter(|l| !l.is_empty() || l.starts_with("//"))
      .map(|def| {
        def
          .split_once("TypedArray.")
          .unwrap()
          .1
          .split("\n")
          .map(|n| n.as_bytes())
          .collect::<Vec<_>>()
      })
      .collect::<Vec<_>>();

    let mut set = HashSet::new();
    for def in DEFS.split("\n") {
      if def.is_empty() || def.starts_with("//") {
        continue;
      };
      if let Some((cls, _)) = def.rsplit_once("(TypedArray)") {
        let cls_def = BuiltinEntry(Builtin(cls.as_bytes(), b"", b""));
        set.insert(cls_def.clone());
        for subpath in typedarray_defs.iter() {
          let mut def = cls_def.clone();
          if let Some(p) = subpath.get(0) {
            def.0 .1 = p;
          };
          if let Some(p) = subpath.get(1) {
            def.0 .2 = p;
          };
          set.insert(def);
        }
      } else {
        let mut builtin = BuiltinEntry(Builtin(b"", b"", b""));
        let mut parts = def.split(".");
        builtin.0 .0 = parts.next().unwrap().as_bytes();
        if let Some(p) = parts.next() {
          builtin.0 .1 = p.as_bytes();
        };
        if let Some(p) = parts.next() {
          builtin.0 .2 = p.as_bytes();
        };
        set.insert(builtin);
      };
    }
    set
  };
}
