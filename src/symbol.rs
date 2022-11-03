use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use crate::ast::NodeId;
use crate::error::SyntaxResult;
use crate::source::SourceRange;

pub type Identifier = SourceRange;

// ScopeId, index in symbol_declaration_order.
pub struct SymbolId(usize, usize);

impl SymbolId {
    pub fn scope_id(&self) -> usize {
        self.0
    }

    pub fn ordinal_in_scope(&self) -> usize {
        self.1
    }
}

#[derive(Clone, Debug)]
pub struct Symbol {
    // This should refer to an ObjectPatternProperty if shorthand property, FunctionName if function name, or IdentifierPattern otherwise.
    declarator_pattern: NodeId,
}

impl Symbol {
    pub fn new(declarator_pattern: NodeId) -> Symbol {
        Symbol { declarator_pattern }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ScopeType {
    Global,
    // Function, or top-level if module.
    Closure,
    Block,
}

#[derive(Debug)]
pub struct ScopeData {
    id: ScopeId,
    symbols: HashMap<Identifier, Symbol>,
    // For deterministic outputs.
    symbol_declaration_order: Vec<Identifier>,
    // Nearest ancestor. Does not exist for top-level.
    // NOTE: Self could be a closure.
    ancestor_closure: Option<ScopeId>,
    // Does not exist for top-level.
    parent: Option<ScopeId>,
    typ: ScopeType,
    is_module_closure: bool,
}

impl ScopeData {
    pub fn is_module_closure(&self) -> bool {
        self.is_module_closure
    }

    pub fn self_or_ancestor_closure(&self) -> Option<ScopeId> {
        if self.typ == ScopeType::Closure {
            Some(self.id)
        } else {
            self.ancestor_closure
        }
    }

    pub fn add_symbol(&mut self, identifier: Identifier, symbol: Symbol) -> SyntaxResult<()> {
        if self.symbols.insert(identifier.clone(), symbol).is_some() {
            // TODO Investigate raising an error; however, many production codebases redeclare `var`.
        } else {
            self.symbol_declaration_order.push(identifier);
        };
        Ok(())
    }

    pub fn add_block_symbol(&mut self, identifier: Identifier, symbol: Symbol) -> SyntaxResult<()> {
        if self.typ != ScopeType::Global {
            self.add_symbol(identifier, symbol)?;
        };
        Ok(())
    }

    pub fn find_symbol<'a>(
        &'a self,
        scope_map: &'a ScopeMap,
        identifier: &'a Identifier,
    ) -> Option<&'a Symbol> {
        match self.symbols.get(identifier) {
            Some(symbol) => Some(symbol),
            None => match self.parent {
                Some(parent_id) => scope_map[parent_id].find_symbol(scope_map, identifier),
                None => None,
            },
        }
    }

    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
    }

    pub fn symbols_update<F: FnMut(usize, &mut Symbol) -> ()>(&mut self, mut f: F) -> () {
        for i in 0..self.symbol_declaration_order.len() {
            let name = &self.symbol_declaration_order[i];
            let symbol = self.symbols.get_mut(name).unwrap();
            f(i, symbol);
        }
    }

    pub fn parent(&self) -> Option<ScopeId> {
        self.parent
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeId(usize);

impl ScopeId {
    pub fn id(&self) -> usize {
        self.0
    }
}

pub struct ScopeMap {
    scopes: Vec<ScopeData>,
}

impl ScopeMap {
    pub fn new() -> ScopeMap {
        ScopeMap { scopes: Vec::new() }
    }

    pub fn create_scope(
        &mut self,
        ancestor_closure: Option<ScopeId>,
        parent: Option<ScopeId>,
        typ: ScopeType,
        is_module_closure: bool,
    ) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(ScopeData {
            id,
            symbols: HashMap::new(),
            symbol_declaration_order: Vec::new(),
            ancestor_closure,
            parent,
            typ,
            is_module_closure,
        });
        id
    }

    pub fn iter(&mut self) -> impl Iterator<Item = &ScopeData> {
        self.scopes.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut ScopeData> {
        self.scopes.iter_mut()
    }

    pub fn len(&self) -> usize {
        self.scopes.len()
    }
}

impl Index<ScopeId> for ScopeMap {
    type Output = ScopeData;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index.0]
    }
}

impl IndexMut<ScopeId> for ScopeMap {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scopes[index.0]
    }
}

impl Index<SymbolId> for ScopeMap {
    type Output = Symbol;

    fn index(&self, index: SymbolId) -> &Self::Output {
        let scope = &self.scopes[index.0];
        &scope.symbols[&scope.symbol_declaration_order[index.1]]
    }
}

/// Use this to associate additional state with a Symbol, identified by a SymbolId.
/// We prefer this instead of adding an extra generic state field on Symbol, as that would require propagating the generic type everywhere.
/// Use as many of these as desired. To allow for initialisation and referencing without Option, the state value must have a default.
pub struct SymbolMap<T> {
    data: Vec<Vec<T>>,
}

impl<T> SymbolMap<T> {
    pub fn new<U: Default>(scope_map: &ScopeMap) -> SymbolMap<U> {
        SymbolMap {
            data: scope_map
                .scopes
                .iter()
                .map(|s| {
                    s.symbol_declaration_order
                        .iter()
                        .map(|_| U::default())
                        .collect()
                })
                .collect(),
        }
    }
}

impl<T> Index<SymbolId> for SymbolMap<T> {
    type Output = T;

    fn index(&self, index: SymbolId) -> &Self::Output {
        &self.data[index.0][index.1]
    }
}

impl<T> IndexMut<SymbolId> for SymbolMap<T> {
    fn index_mut(&mut self, index: SymbolId) -> &mut Self::Output {
        &mut self.data[index.0][index.1]
    }
}
