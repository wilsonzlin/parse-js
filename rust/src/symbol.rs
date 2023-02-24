use crate::error::SyntaxResult;
use crate::session::Session;
use crate::session::SessionHashMap;
use crate::session::SessionVec;
use crate::source::SourceRange;
use core::ops::BitOr;
use hashbrown::hash_map::Entry;
use std::cell::Cell;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::hash::Hash;
use std::rc::Rc;

pub type Identifier<'a> = SourceRange<'a>;

// We don't store the associated Scope anymore as we want to allow easy moving of symbols between scopes (the parser doesn't do this, but library consumers might), which allows for easy migration of usages without having to rename every one of them. Since we don't have anything else to store, we can't use a reference due to potential zero-sized allocation issues, so we just use a unique sequence number instead.
// To attach additional custom state to a Symbol, use a HashMap. We prefer this instead of adding an extra generic state field on Symbol, as that would require propagating the generic type everywhere.
// Cloning means to cheaply clone the reference to this unique symbol, not create a duplicate symbol. This is useful for sharing a reference to a symbol, including uses in data structures like HashMap.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u64);

#[derive(Clone)]
pub struct SymbolGenerator(Rc<Cell<u64>>);

impl SymbolGenerator {
  pub fn new() -> SymbolGenerator {
    SymbolGenerator(Rc::new(Cell::new(0)))
  }

  pub fn next(&self) -> Symbol {
    let id = self.0.get();
    self.0.set(id + 1);
    Symbol(id)
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ScopeType {
  Global,
  Module,
  // Closure with `this` (property initialisers have access to it) but not `arguments`.
  Class,
  // Functions with `this` and `arguments`.
  NonArrowFunction,
  // Functions with neither `this` nor `arguments`.
  // NOTE: Arrow function class properties are not on the prototype and simply have access to the class's `this` like other initialisers; it doesn't have a special `this` binding and inherits it like any other arrow function.
  ArrowFunction,
  Block,
}

impl ScopeType {
  pub fn is_closure(&self) -> bool {
    match self {
      ScopeType::Module => true,
      ScopeType::NonArrowFunction => true,
      ScopeType::ArrowFunction => true,
      _ => false,
    }
  }

  pub fn is_closure_or_global(&self) -> bool {
    match self {
      ScopeType::Global => true,
      t => t.is_closure(),
    }
  }

  pub fn is_closure_or_block(&self) -> bool {
    match self {
      ScopeType::Block => true,
      t => t.is_closure(),
    }
  }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScopeFlag {
  // Whether or not this scope has a `this` expression. Only applicable for Class and NonArrowFunction scopes.
  UsesThis,
  // Whether or not this scope has an `arguments` variable expression. Only applicable for NonArrowFunction scopes.
  UsesArguments,
}

impl ScopeFlag {
  fn bitfield(self) -> u64 {
    1 << (self as u8)
  }
}

pub struct ScopeFlags(u64);

impl BitOr<ScopeFlag> for ScopeFlag {
  type Output = ScopeFlags;

  fn bitor(self, rhs: Self) -> Self::Output {
    ScopeFlags(self.bitfield() | rhs.bitfield())
  }
}

impl BitOr<ScopeFlags> for ScopeFlag {
  type Output = ScopeFlags;

  fn bitor(self, rhs: ScopeFlags) -> Self::Output {
    ScopeFlags(self.bitfield() | rhs.0)
  }
}

impl BitOr<ScopeFlag> for ScopeFlags {
  type Output = ScopeFlags;

  fn bitor(self, rhs: ScopeFlag) -> Self::Output {
    ScopeFlags(self.0 | rhs.bitfield())
  }
}

impl BitOr<ScopeFlags> for ScopeFlags {
  type Output = ScopeFlags;

  fn bitor(self, rhs: Self) -> Self::Output {
    ScopeFlags(self.0 | rhs.0)
  }
}

struct ScopeData<'a> {
  symbol_generator: SymbolGenerator,
  symbols: SessionHashMap<'a, Identifier<'a>, Symbol>,
  // For deterministic outputs, and to give each Symbol an ID.
  symbol_declaration_order: SessionVec<'a, Identifier<'a>>,
  // Does not exist for top-level.
  parent: Option<Scope<'a>>,
  // Not used by the parser, but useful for some library consumers, as there's currently no other way to iterate all scopes.
  children: SessionVec<'a, Scope<'a>>,
  typ: ScopeType,
  flags: u64,
}

// Since Rust only supports newtypes with impls using structs, we cannot have our newtype as a reference, so we must use it like it's a reference despite being a struct i.e. cheaply copyable, take `self` instead of `&self`, use `Scope<'a>` instead of `&'a Scope<'a>`.
#[derive(Clone, Copy)]
pub struct Scope<'a>(&'a RefCell<ScopeData<'a>>);

impl<'a> Scope<'a> {
  fn get<'b>(self) -> Ref<'b, ScopeData<'a>> {
    self.0.borrow()
  }

  fn get_mut<'b>(self) -> RefMut<'b, ScopeData<'a>> {
    self.0.borrow_mut()
  }

  pub fn new(
    session: &'a Session,
    symbol_generator: SymbolGenerator,
    parent: Option<Scope<'a>>,
    typ: ScopeType,
  ) -> Scope<'a> {
    let scope = Scope(session.mem.alloc(RefCell::new(ScopeData {
      symbol_generator,
      symbols: session.new_hashmap(),
      symbol_declaration_order: session.new_vec(),
      parent,
      children: session.new_vec(),
      typ,
      flags: 0,
    })));
    if let Some(parent) = parent {
      parent.get_mut().children.push(scope);
    };
    scope
  }

  pub fn parent(self) -> Option<Scope<'a>> {
    self.get().parent
  }

  pub fn typ(self) -> ScopeType {
    self.get().typ
  }

  pub fn create_child_scope(self, session: &'a Session, typ: ScopeType) -> Scope<'a> {
    Scope::new(
      session,
      self.get().symbol_generator.clone(),
      Some(self),
      typ,
    )
  }

  pub fn find_self_or_ancestor<F: Fn(ScopeType) -> bool>(self, pred: F) -> Option<Scope<'a>> {
    let cur = self.get();
    if pred(cur.typ) {
      Some(self)
    } else if let Some(parent) = cur.parent {
      parent.find_self_or_ancestor(pred)
    } else {
      None
    }
  }

  pub fn find_furthest_self_or_ancestor<F: Fn(ScopeType) -> bool>(
    self,
    pred: F,
  ) -> Option<Scope<'a>> {
    let mut latest_match = None;
    let mut cur = self;
    loop {
      let scope = cur.get();
      if pred(scope.typ) {
        latest_match = Some(cur);
      } else {
        break;
      }
      let Some(parent) = scope.parent else {
        break;
      };
      cur = parent;
    }
    latest_match
  }

  pub fn has_flag(&self, flag: ScopeFlag) -> bool {
    (self.get().flags & flag.bitfield()) != 0
  }

  pub fn has_any_of_flags(&self, q: ScopeFlags) -> bool {
    (self.get().flags & q.0) != 0
  }

  pub fn has_all_of_flags(&self, q: ScopeFlags) -> bool {
    (!self.get().flags & q.0) == 0
  }

  pub fn set_flag(&mut self, flag: ScopeFlag) {
    self.get_mut().flags |= 1 << (flag as u8);
  }

  pub fn add_symbol(self, identifier: Identifier<'a>) -> SyntaxResult<'a, ()> {
    // We must get before we borrow as mut, even if we won't use it.
    let ordinal_in_scope = self.get().symbol_declaration_order.len();
    let mut as_mut = self.get_mut();
    match as_mut.symbols.entry(identifier.clone()) {
      Entry::Occupied(_) => {
        // Do not replace existing entry, as it has associated index in symbol_declaration_order.
        // TODO Investigate raising an error; however, many production codebases redeclare `var`.
      }
      Entry::Vacant(e) => {
        e.insert(self.get().symbol_generator.next());
        as_mut.symbol_declaration_order.push(identifier.clone());
      }
    };
    Ok(())
  }

  pub fn add_block_symbol(self, identifier: Identifier<'a>) -> SyntaxResult<'a, ()> {
    if self.get().typ != ScopeType::Global {
      self.add_symbol(identifier)?;
    };
    Ok(())
  }

  pub fn get_symbol(self, identifier: Identifier<'a>) -> Option<Symbol> {
    self.get().symbols.get(&identifier).cloned()
  }

  pub fn find_symbol(self, identifier: Identifier<'a>) -> Option<Symbol> {
    match self.get().symbols.get(&identifier) {
      Some(symbol) => Some(symbol.clone()),
      None => match self.get().parent {
        Some(parent) => parent.find_symbol(identifier),
        None => None,
      },
    }
  }

  pub fn find_symbol_up_to_nearest_scope_of_type<'b>(
    self,
    identifier: Identifier<'a>,
    scope_type: ScopeType,
  ) -> Option<Symbol> {
    let mut scope = self;
    loop {
      let scope_data = scope.get();
      if let Some(symbol) = scope_data.symbols.get(&identifier) {
        return Some(symbol.clone());
      };
      if scope_data.typ == scope_type {
        break;
      };
      if let Some(parent) = scope_data.parent {
        scope = parent;
        continue;
      };
      break;
    }
    None
  }

  pub fn symbol_count(self) -> usize {
    self.get().symbols.len()
  }

  // Returning an iterator within a Ref is difficult (see // https://stackoverflow.com/a/33542412/6249022), so we just return the Vec (immutable) instead.
  pub fn symbol_names<'b>(self) -> Ref<'b, SessionVec<'a, SourceRange<'a>>> {
    Ref::map(self.get(), |scope| &scope.symbol_declaration_order)
  }

  // Same return value rationale as `symbol_names`.
  pub fn children<'b>(self) -> Ref<'b, SessionVec<'a, Scope<'a>>> {
    Ref::map(self.get(), |scope| &scope.children)
  }
}

// Equality means referring to the same unique scope. Useful for HashMap.
impl<'a> PartialEq for Scope<'a> {
  fn eq(&self, other: &Self) -> bool {
    core::ptr::eq(self.0, other.0)
  }
}

impl<'a> Eq for Scope<'a> {}

impl<'a> Hash for Scope<'a> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    core::ptr::hash(self.0, state);
  }
}
