use std::marker::PhantomData;
use std::ops::BitOr;
use std::ops::BitOrAssign;

pub trait Flag {
  // This should always be the same code; use `flag_bitfield!(self)`. This trait exists to allow for generic F in Flags<F>, not for shared interfaces.
  fn bitfield(self) -> u64;
}

macro_rules! flag_bitfield {
  ($f:expr) => {
    1 << ($f as u8)
  };
}

pub(crate) use flag_bitfield;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flags<F: Flag>(u64, PhantomData<F>);

impl<F: Flag> Flags<F> {
  pub const fn from_raw(raw: u64) -> Self {
    Self(0, PhantomData)
  }

  pub fn new() -> Self {
    Self(0, PhantomData)
  }

  pub fn select(self, flags: Flags<F>) -> Flags<F> {
    Self(self.0 & flags.0, PhantomData)
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
