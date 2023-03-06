use std::marker::PhantomData;
use std::ops::BitOr;
use std::ops::BitOrAssign;

pub trait Flag {
  fn bitfield(self) -> u64;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flags<F: Flag>(u64, PhantomData<F>);

impl<F: Flag> Flags<F> {
  pub fn new() -> Self {
    Self(0, PhantomData)
  }

  pub fn has(self, flag: F) -> bool {
    self.0 & flag.bitfield() != 0
  }

  pub fn has_any(self, flags: Flags<F>) -> bool {
    self.0 & flags.0 != 0
  }

  pub fn has_all(self, flags: Flags<F>) -> bool {
    (!self.0 & flags.0) == 0
  }

  pub fn set(&mut self, flag: F) -> &mut Self {
    self.0 |= flag.bitfield();
    self
  }

  pub fn set_all(&mut self, flags: Flags<F>) -> &mut Self {
    self.0 |= flags.0;
    self
  }
}

impl<F: Flag> BitOr<F> for Flags<F> {
  type Output = Flags<F>;

  fn bitor(self, rhs: F) -> Self::Output {
    Flags(self.0 | rhs.bitfield(), PhantomData)
  }
}

impl<F: Flag> BitOr<Flags<F>> for Flags<F> {
  type Output = Flags<F>;

  fn bitor(self, rhs: Self) -> Self::Output {
    Flags(self.0 | rhs.0, PhantomData)
  }
}

impl<F: Flag> BitOrAssign<F> for Flags<F> {
  fn bitor_assign(&mut self, rhs: F) {
    self.set(rhs);
  }
}

impl<F: Flag> BitOrAssign<Flags<F>> for Flags<F> {
  fn bitor_assign(&mut self, rhs: Flags<F>) {
    self.set_all(rhs);
  }
}
