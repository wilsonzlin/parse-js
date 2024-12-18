use crate::symbol::Scope;
use crate::symbol::ScopeType;
use parse_js::ast::Node;
use parse_js::ast::Syntax;
use parse_js::ast::VarDeclMode;
use parse_js::visit::JourneyControls;
use parse_js::visit::VisitorMut;
use std::mem::replace;

#[derive(PartialEq, Eq, Clone, Copy)]
enum AddToScope {
  IfNotGlobal,
  NearestClosure,
}

pub(crate) struct DeclVisitor {
  scope: Scope,
  scope_stack: Vec<Scope>,
  pattern_action: Option<AddToScope>,
  pattern_action_stack: Vec<Option<AddToScope>>,
}

impl DeclVisitor {
  pub fn new(top_level_scope: Scope) -> DeclVisitor {
    Self {
      scope: top_level_scope,
      scope_stack: Vec::new(),
      pattern_action: None,
      pattern_action_stack: Vec::new(),
    }
  }

  fn add_to_scope(&mut self, name: String, action: AddToScope) {
    match action {
      AddToScope::IfNotGlobal => {
        self.scope.data_mut().add_symbol_if_not_global(name);
      }
      AddToScope::NearestClosure => {
        if let Some(closure) = self.scope.find_nearest_scope(|t| t.is_closure()) {
          closure.data_mut().add_symbol(name);
        };
      }
    };
  }

  fn new_scope(&mut self, new_scope_type: ScopeType) {
    let new_scope = self.scope.create_child_scope(new_scope_type);
    let old_scope = replace(&mut self.scope, new_scope);
    self.scope_stack.push(old_scope);
  }

  fn restore_scope(&mut self) {
    let old_scope = self.scope_stack.pop().unwrap();
    self.scope = old_scope;
  }

  fn new_pattern_action(&mut self, new_pattern_action: AddToScope) {
    let old_action = self.pattern_action.replace(new_pattern_action);
    self.pattern_action_stack.push(old_action);
  }

  fn restore_pattern_action(&mut self) {
    let old_action = self.pattern_action_stack.pop().unwrap();
    self.pattern_action = old_action;
  }
}

impl VisitorMut for DeclVisitor {
  fn on_syntax_down(&mut self, node: &mut Node, _ctl: &mut JourneyControls) {
    match node.stx.as_ref() {
      Syntax::BlockStmt { .. } => {
        self.new_scope(ScopeType::Block);
      }
      Syntax::CatchBlock { .. } => {
        self.new_scope(ScopeType::Block);
        // For the parameter.
        self.new_pattern_action(AddToScope::IfNotGlobal);
      }
      Syntax::ClassDecl { name, .. } => {
        if let Some(name) = name {
          let Syntax::ClassOrFunctionName { name } = name.stx.as_ref() else {
            unreachable!();
          };
          self.add_to_scope(name.clone(), AddToScope::IfNotGlobal);
        };
        self.new_scope(ScopeType::Class);
      }
      Syntax::ClassExpr { name, .. } => {
        // The name belongs to the new Class scope (unlike a ClassDecl).
        self.new_scope(ScopeType::Class);
        if let Some(name) = name {
          let Syntax::ClassOrFunctionName { name } = name.stx.as_ref() else {
            unreachable!();
          };
          self.add_to_scope(name.clone(), AddToScope::IfNotGlobal);
        };
      }
      Syntax::ForInStmt { .. } | Syntax::ForOfStmt { .. } | Syntax::ForStmt { .. } => {
        // For any declarators in the initialiser.
        self.new_scope(ScopeType::Block);
      }
      Syntax::Function { arrow, .. } => {
        if *arrow {
          self.new_scope(ScopeType::ArrowFunction);
        } else {
          self.new_scope(ScopeType::NonArrowFunction);
        }
        // For the parameters.
        self.new_pattern_action(AddToScope::NearestClosure);
      }
      Syntax::FunctionDecl { name, .. } => {
        // WARNING: The name belongs in the containing scope, not the function's scope.
        // See examples/function.js.
        if let Some(name) = name {
          let Syntax::ClassOrFunctionName { name } = name.stx.as_ref() else {
            unreachable!();
          };
          self.add_to_scope(name.clone(), AddToScope::NearestClosure);
        };
      }
      Syntax::FunctionExpr { name, .. } => {
        // We need to create a new scope just for the name itself. Unlike function declarations, function expressions are not declared within their current closure or block. However, their names cannot be assigned to within the function (it has no effect in non-strict mode) and they can be "redeclared" e.g. `(function a() { let a = 1; })()`. See examples/function.js.
        // TODO Is NonArrowFunction the best choice?
        self.new_scope(ScopeType::NonArrowFunction);
        if let Some(name) = name {
          let Syntax::ClassOrFunctionName { name } = name.stx.as_ref() else {
            unreachable!();
          };
          self.add_to_scope(name.clone(), AddToScope::IfNotGlobal);
        };
      }
      Syntax::IdentifierPattern { name } => {
        // An identifier pattern doesn't always mean declaration e.g. simple assignment.
        if let Some(pattern_action) = self.pattern_action {
          self.add_to_scope(name.clone(), pattern_action);
        }
      }
      Syntax::ImportStmt { .. } => {
        self.new_pattern_action(AddToScope::IfNotGlobal);
      }
      Syntax::VarDecl { mode, .. } => {
        self.new_pattern_action(match mode {
          VarDeclMode::Const => AddToScope::IfNotGlobal,
          VarDeclMode::Let => AddToScope::IfNotGlobal,
          VarDeclMode::Var => AddToScope::NearestClosure,
        });
      }
      _ => {}
    };
    node.assoc.set(self.scope.clone());
  }

  fn on_syntax_up(&mut self, node: &mut Node) {
    match node.stx.as_ref() {
      Syntax::BlockStmt { .. }
      | Syntax::CatchBlock { .. }
      | Syntax::ClassDecl { .. }
      | Syntax::ClassExpr { .. }
      | Syntax::ForStmt { .. }
      | Syntax::ForInStmt { .. }
      | Syntax::ForOfStmt { .. }
      | Syntax::Function { .. }
      | Syntax::FunctionExpr { .. } => {
        self.restore_scope();
      }
      _ => {}
    };
    match node.stx.as_ref() {
      Syntax::CatchBlock { .. }
      | Syntax::Function { .. }
      | Syntax::ImportStmt { .. }
      | Syntax::VarDecl { .. } => {
        self.restore_pattern_action();
      }
      _ => {}
    };
  }
}
