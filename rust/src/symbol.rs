use crate::ast::Node;
use crate::error::SyntaxResult;
use crate::session::Session;
use crate::session::SessionHashMap;
use crate::session::SessionVec;
use crate::source::SourceRange;
use hashbrown::hash_map::Entry;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::slice::Iter;

pub type Identifier<'a> = SourceRange<'a>;

// To attach additional custom state to a Symbol, use a HashMap. We prefer this instead of adding an extra generic state field on Symbol, as that would require propagating the generic type everywhere.
// Cloning means to cheaply clone the reference to this unique symbol, not create a duplicate symbol.
#[derive(Clone)]
pub struct Symbol<'a> {
  scope: Scope<'a>,
  // Index in the containing ScopeData's symbol_declaration_order Vec.
  ordinal_in_scope: usize,
  // This should refer to an ObjectPatternProperty if shorthand property, FunctionName if function name, or IdentifierPattern otherwise.
  declarator_pattern: Node<'a>,
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
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ScopeFlag {
  // Whether or not this scope has a `this` expression. Only applicable for Class and NonArrowFunction scopes.
  UsesThis,
  // Whether or not this scope has an `arguments` variable expression. Only applicable for NonArrowFunction scopes.
  UsesArguments,
}

struct ScopeData<'a> {
  symbols: SessionHashMap<'a, Identifier<'a>, Symbol<'a>>,
  // For deterministic outputs, and to give each Symbol an ID.
  symbol_declaration_order: SessionVec<'a, Identifier<'a>>,
  // Does not exist for top-level.
  parent: Option<Scope<'a>>,
  typ: ScopeType,
  flags: u64,
}

pub struct ScopeSymbolNames<'a, 'b> {
  r: Ref<'b, ScopeData<'a>>,
}

// https://stackoverflow.com/a/33542412/6249022
impl<'a, 'b, 'c: 'b> IntoIterator for &'c ScopeSymbolNames<'a, 'b> {
  type IntoIter = Iter<'b, Identifier<'a>>;
  type Item = &'b Identifier<'a>;

  fn into_iter(self) -> Self::IntoIter {
    self.r.symbol_declaration_order.iter()
  }
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

  pub fn new(session: &'a Session, parent: Option<Scope<'a>>, typ: ScopeType) -> Scope<'a> {
    Scope(session.mem.alloc(RefCell::new(ScopeData {
      symbols: session.new_hashmap(),
      symbol_declaration_order: session.new_vec(),
      parent,
      typ,
      flags: 0,
    })))
  }

  pub fn create_child_scope(self, session: &'a Session, typ: ScopeType) -> Scope<'a> {
    Scope::new(session, Some(self), typ)
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

  pub fn has_flag(&self, flag: ScopeFlag) -> bool {
    (self.get().flags & (1 << (flag as u8))) != 0
  }

  pub fn set_flag(&mut self, flag: ScopeFlag) {
    self.get_mut().flags |= 1 << (flag as u8);
  }

  pub fn add_symbol(
    self,
    identifier: Identifier<'a>,
    declarator_pattern: Node<'a>,
  ) -> SyntaxResult<'a, ()> {
    // We must get before we borrow as mut, even if we won't use it.
    let ordinal_in_scope = self.get().symbol_declaration_order.len();
    let mut as_mut = self.get_mut();
    match as_mut.symbols.entry(identifier.clone()) {
      Entry::Occupied(_) => {
        // Do not replace existing entry, as it has associated index in symbol_declaration_order.
        // TODO Investigate raising an error; however, many production codebases redeclare `var`.
      }
      Entry::Vacant(e) => {
        e.insert(Symbol {
          scope: self,
          declarator_pattern,
          ordinal_in_scope,
        });
        as_mut.symbol_declaration_order.push(identifier.clone());
      }
    };
    Ok(())
  }

  pub fn add_block_symbol(
    self,
    identifier: Identifier<'a>,
    declarator_pattern: Node<'a>,
  ) -> SyntaxResult<'a, ()> {
    if self.get().typ != ScopeType::Global {
      self.add_symbol(identifier, declarator_pattern)?;
    };
    Ok(())
  }

  pub fn find_symbol(self, identifier: Identifier<'a>) -> Option<Symbol<'a>> {
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
  ) -> Option<Symbol<'a>> {
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

  pub fn symbol_names<'b>(self) -> ScopeSymbolNames<'a, 'b> {
    ScopeSymbolNames { r: self.get() }
  }
}
