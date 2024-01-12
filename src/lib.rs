use ahash::AHashMap;
use core::ptr;
use std::cell::Cell;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::rc::Rc;

pub type Identifier = String;

// We don't store the associated Scope anymore as we want to allow easy moving of symbols between scopes (the parser doesn't do this, but library consumers might), which allows for easy migration of usages without having to rename every one of them. Since we don't have anything else to store, we can't use a reference due to potential zero-sized allocation issues, so we just use a unique sequence number instead.
// To attach additional custom state to a Symbol, use a HashMap. We prefer this instead of adding an extra generic state field on Symbol, as that would require propagating the generic type everywhere.
// Cloning means to cheaply clone the reference to this unique symbol, not create a duplicate symbol. This is useful for sharing a reference to a symbol, including uses in data structures like HashMap.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Symbol(u64);

// TODO We can probably avoid using a Rc, but this would require refs and lifetimes (possibly not 'a but an additional one) everywhere. Investigate if performance becomes costly.
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

  pub fn is_closure_or_class(&self) -> bool {
    match self {
      ScopeType::Class => true,
      t => t.is_closure(),
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

pub struct ScopeData {
  symbol_generator: SymbolGenerator,
  symbols: AHashMap<Identifier, Symbol>,
  // For deterministic outputs, and to give each Symbol an ID.
  symbol_declaration_order: Vec<Identifier>,
  // Does not exist for top-level.
  parent: Option<Scope>,
  // Not used by the parser, but useful for some library consumers, as there's currently no other way to iterate all scopes.
  children: Vec<Scope>,
  typ: ScopeType,
}

impl ScopeData {
  pub fn parent(&self) -> Option<&Scope> {
    self.parent.as_ref()
  }

  pub fn typ(&self) -> ScopeType {
    self.typ
  }

  pub fn add_symbol(&mut self, identifier: Identifier) {
    match self.symbols.entry(identifier.clone()) {
      Entry::Occupied(_) => {
        // Do not replace existing entry, as it has associated index in symbol_declaration_order.
        // TODO Investigate raising an error; however, many production codebases redeclare `var`.
      }
      Entry::Vacant(e) => {
        e.insert(self.symbol_generator.next());
        self.symbol_declaration_order.push(identifier.clone());
      }
    };
  }

  pub fn add_block_symbol(&mut self, identifier: Identifier) {
    if self.typ != ScopeType::Global {
      self.add_symbol(identifier);
    };
  }

  pub fn get_symbol(&self, identifier: Identifier) -> Option<Symbol> {
    self.symbols.get(&identifier).cloned()
  }

  pub fn symbol_count(&self) -> usize {
    self.symbols.len()
  }

  pub fn symbol_names(&self) -> &Vec<String> {
    &self.symbol_declaration_order
  }

  pub fn children(&self) -> &Vec<Scope> {
    &self.children
  }
}

#[derive(Clone)]
pub struct Scope(Rc<RefCell<ScopeData>>);

impl Scope {
  pub fn new(symbol_generator: SymbolGenerator, parent: Option<Scope>, typ: ScopeType) -> Scope {
    let scope = Scope(Rc::new(RefCell::new(ScopeData {
      symbol_generator,
      symbols: AHashMap::new(),
      symbol_declaration_order: Vec::new(),
      parent: parent.clone(),
      children: Vec::new(),
      typ,
    })));
    if let Some(parent) = parent {
      parent.0.borrow_mut().children.push(scope.clone());
    };
    scope
  }

  pub fn data(&self) -> Ref<'_, ScopeData> {
    self.0.borrow()
  }

  pub fn data_mut(&self) -> RefMut<'_, ScopeData> {
    self.0.borrow_mut()
  }

  pub fn create_child_scope(&self, typ: ScopeType) -> Scope {
    // Scope::new will also acquire ref, so we cannot do this inline.
    let symbol_generator = self.0.borrow().symbol_generator.clone();
    Scope::new(symbol_generator, Some(self.clone()), typ)
  }

  pub fn find_nearest_self_or_ancestor<F: Fn(ScopeType) -> bool>(&self, pred: F) -> Option<Scope> {
    let cur = self.0.borrow();
    if pred(cur.typ) {
      Some(self.clone())
    } else if let Some(parent) = &cur.parent {
      parent.find_nearest_self_or_ancestor(pred)
    } else {
      None
    }
  }

  pub fn find_furthest_self_or_ancestor<F: Fn(ScopeType) -> bool>(&self, pred: F) -> Option<Scope> {
    let mut latest_match = None;
    let mut cur = self.clone();
    loop {
      if !pred(cur.0.borrow().typ) {
        break;
      };
      latest_match = Some(cur.clone());
      let Some(parent) = cur.0.borrow().parent.clone() else {
        break;
      };
      cur = parent;
    }
    latest_match
  }

  pub fn find_symbol_up_to_with_scope(
    &self,
    identifier: Identifier,
    scope_pred: impl Fn(ScopeType) -> bool,
  ) -> Option<(Scope, Symbol)> {
    let cur = self.0.borrow();
    match cur.symbols.get(&identifier) {
      Some(symbol) => Some((self.clone(), symbol.clone())),
      None => {
        if scope_pred(cur.typ) {
          None
        } else {
          match &cur.parent {
            Some(parent) => parent.find_symbol_up_to_with_scope(identifier, scope_pred),
            None => None,
          }
        }
      }
    }
  }

  pub fn find_symbol_up_to<'b>(
    &self,
    identifier: Identifier,
    scope_pred: impl Fn(ScopeType) -> bool,
  ) -> Option<Symbol> {
    self
      .find_symbol_up_to_with_scope(identifier, scope_pred)
      .map(|(_, symbol)| symbol)
  }

  pub fn find_symbol_up_to_nearest_scope_of_type<'b>(
    &self,
    identifier: Identifier,
    scope_type: ScopeType,
  ) -> Option<Symbol> {
    self.find_symbol_up_to(identifier, |t| t == scope_type)
  }

  pub fn find_symbol(&self, identifier: Identifier) -> Option<Symbol> {
    self.find_symbol_up_to(identifier, |_| false)
  }
}

// Equality means referring to the same unique scope. Useful for HashMap.
impl PartialEq for Scope {
  fn eq(&self, other: &Self) -> bool {
    ptr::eq(self.0.as_ptr(), other.0.as_ptr())
  }
}

impl Eq for Scope {}

impl Hash for Scope {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    ptr::hash(self.0.as_ptr(), state);
  }
}
