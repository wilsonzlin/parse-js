use bumpalo::Bump;
use core::fmt::Debug;
use core::hash::Hash;
use core::ops::Deref;
use core::ops::DerefMut;
use hashbrown::hash_map::DefaultHashBuilder;
use hashbrown::BumpWrapper;
#[cfg(feature = "serialize")]
use serde::ser::SerializeSeq;
use std::fmt;
use std::fmt::Formatter;

// Some of these are newtypes so that we can implement Serialize.

pub type SessionHashMap<'a, K, V> = hashbrown::HashMap<K, V, DefaultHashBuilder, BumpWrapper<'a>>;

pub type SessionHashSet<'a, T> = hashbrown::HashSet<T, DefaultHashBuilder, BumpWrapper<'a>>;

#[derive(PartialEq, Eq, Clone)]
pub struct SessionVec<'a, T>(bumpalo::collections::Vec<'a, T>);

impl<'a, T: Debug> Debug for SessionVec<'a, T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl<'a, T> Deref for SessionVec<'a, T> {
  type Target = bumpalo::collections::Vec<'a, T>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<'a, T> DerefMut for SessionVec<'a, T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<'a, T> IntoIterator for SessionVec<'a, T> {
  type IntoIter = bumpalo::collections::vec::IntoIter<'a, T>;
  type Item = T;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<'a, 'item, T> IntoIterator for &'item SessionVec<'a, T> {
  type IntoIter = core::slice::Iter<'item, T>;
  type Item = &'item T;

  fn into_iter(self) -> Self::IntoIter {
    self.0.iter()
  }
}

impl<'a, 'item, T> IntoIterator for &'item mut SessionVec<'a, T> {
  type IntoIter = core::slice::IterMut<'item, T>;
  type Item = &'item mut T;

  fn into_iter(self) -> Self::IntoIter {
    self.0.iter_mut()
  }
}

#[cfg(feature = "serialize")]
impl<'a, T: serde::Serialize> serde::Serialize for SessionVec<'a, T> {
  fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
    for e in self.0.iter() {
      seq.serialize_element(e)?;
    }
    seq.end()
  }
}

pub struct SessionString<'a>(bumpalo::collections::String<'a>);

impl<'a> Debug for SessionString<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.0.fmt(f)
  }
}

impl<'a> Deref for SessionString<'a> {
  type Target = bumpalo::collections::String<'a>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<'a> DerefMut for SessionString<'a> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

#[cfg(feature = "serialize")]
impl<'a> serde::Serialize for SessionString<'a> {
  fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.serialize_str(&self.0)
  }
}

pub struct Session {
  pub(crate) mem: Bump,
}

impl Session {
  pub fn new() -> Session {
    Session { mem: Bump::new() }
  }

  pub fn reset(&mut self) {
    self.mem.reset();
  }

  pub fn get_allocator(&self) -> &Bump {
    &self.mem
  }

  pub fn new_vec<T>(&self) -> SessionVec<T> {
    SessionVec(bumpalo::collections::Vec::new_in(&self.mem))
  }

  pub fn new_string(&self) -> SessionString {
    SessionString(bumpalo::collections::String::new_in(&self.mem))
  }

  pub fn new_hashmap<K, V>(&self) -> SessionHashMap<K, V> {
    SessionHashMap::new_in(BumpWrapper(&self.mem))
  }

  pub fn new_hashset<T: Hash + Eq>(&self) -> SessionHashSet<T> {
    SessionHashSet::new_in(BumpWrapper(&self.mem))
  }
}
