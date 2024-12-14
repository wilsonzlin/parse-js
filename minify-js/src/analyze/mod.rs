use ahash::{AHashMap, AHashSet};
use parse_js::{ast::{Node, Syntax}, loc::Loc, visit::{JourneyControls, Visitor}};
use symbol_js::symbol::{Scope, ScopeType, Symbol};

#[cfg(test)]
mod tests;

// Four tasks (fill out each field as appropriate).
#[derive(Default)]
pub(crate) struct VarVisitor {
  pub declared: AHashSet<Symbol>,
  pub foreign: AHashSet<Symbol>,
  pub unknown: AHashSet<String>,
  pub use_before_decl: AHashMap<Symbol, Loc>,
}

// The lifted scope is the nearest self-or-ancestor scope that is not a block, or the self-or-ancestor scope just below the global scope.
// This is useful as we don't want a usage in an inner block to count as "foreign".
fn lifted_scope(scope: &Scope) -> Scope {
  if scope.data().typ() != ScopeType::Block {
    return scope.clone();
  };
  let scope_data = scope.data();
  let parent = scope_data.parent().unwrap();
  if parent.data().typ() == ScopeType::Global {
    return scope.clone();
  };
  lifted_scope(parent)
}

impl Visitor for VarVisitor {
  fn on_syntax_down(&mut self, node: &Node, ctl: &mut JourneyControls) {
    match node.stx.as_ref() {
      Syntax::IdentifierExpr { name } => {
        let usage_scope = node.assoc.get::<Scope>().unwrap();
        let usage_ls = lifted_scope(usage_scope);
        match usage_scope.find_symbol_up_to_with_scope(name.clone(), |_| false) {
          None => {
            // Unknown.
            self.unknown.insert(name.clone());
          }
          Some((decl_scope, symbol)) => {
            let decl_ls = lifted_scope(&decl_scope);
            if usage_ls != decl_ls {
              self.foreign.insert(symbol);
            } else if !self.declared.contains(&symbol) {
              // Check for use before declaration to ensure strict SSA.
              // NOTE: This doesn't check across closures, as that is mostly a runtime determination (see symbol-js/examples/let.js), but we don't care as those are foreign vars and don't affect strict SSA (i.e. correctness).
              self.use_before_decl.insert(symbol, node.loc);
            }
          }
        };
      }
      Syntax::IdentifierPattern { name } | Syntax::ClassOrFunctionName { name } => {
        let scope = node.assoc.get::<Scope>().unwrap();
        // It won't exist if it's a global declaration.
        // TODO Is this the only time it won't exist (i.e. is it always safe to ignore None)?
        if let Some(symbol) = scope.find_symbol(name.clone()) {
          assert!(self.declared.insert(symbol));
        };
      }
      _ => {}
    }
  }
}
