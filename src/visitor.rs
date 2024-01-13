use crate::symbol::Scope;
use crate::symbol::ScopeType;
use parse_js::ast::ClassOrObjectMemberValue;
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
      Syntax::ArrowFunctionExpr { .. } => {
        self.new_scope(ScopeType::ArrowFunction);
      }
      Syntax::BlockStmt { .. } => {
        // TODO Is creating a new block safe if the block is for a function body? A `let` at a function's top level should not go to a Block, but to a closure. Also, this creates an incorrect additional scope for ForStmt, CatchBlock, etc.
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
      Syntax::ClassMember { value, .. } => match value {
        ClassOrObjectMemberValue::Getter { .. }
        | ClassOrObjectMemberValue::Method { .. }
        | ClassOrObjectMemberValue::Setter { .. } => {
          self.new_scope(ScopeType::NonArrowFunction);
        }
        ClassOrObjectMemberValue::Property { .. } => {}
      },
      Syntax::ForInStmt { .. } | Syntax::ForOfStmt { .. } | Syntax::ForStmt { .. } => {
        // For any declarators in the initialiser.
        self.new_scope(ScopeType::Block);
      }
      Syntax::FunctionDecl { name, .. } => {
        // WARNING: The name belongs in the containing scope, not the function's scope.
        // For example, `function a() { let a = 1; }` is legal.
        if let Some(name) = name {
          let Syntax::ClassOrFunctionName { name } = name.stx.as_ref() else {
            unreachable!();
          };
          self.add_to_scope(name.clone(), AddToScope::NearestClosure);
        };
        self.new_scope(ScopeType::NonArrowFunction);
      }
      Syntax::FunctionExpr { name, .. } => {
        self.new_scope(ScopeType::NonArrowFunction);
        // WARNING: Unlike function declarations, function expressions are not declared within their current closure or block. However, their names cannot be assigned to within the function (it has no effect) and they can be "redeclared" e.g. `(function a() { let a = 1; })()`.
        if let Some(name) = name {
          let Syntax::ClassOrFunctionName { name } = name.stx.as_ref() else {
            unreachable!();
          };
          self.add_to_scope(name.clone(), AddToScope::IfNotGlobal);
        };
      }
      Syntax::FunctionSignature { .. } => {
        self.new_pattern_action(AddToScope::NearestClosure);
      }
      Syntax::IdentifierPattern { name } => {
        self.add_to_scope(name.clone(), self.pattern_action.unwrap());
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
      Syntax::ArrowFunctionExpr { .. }
      | Syntax::BlockStmt { .. }
      | Syntax::CatchBlock { .. }
      | Syntax::ClassDecl { .. }
      | Syntax::ClassExpr { .. }
      | Syntax::ClassMember {
        value:
          ClassOrObjectMemberValue::Getter { .. }
          | ClassOrObjectMemberValue::Method { .. }
          | ClassOrObjectMemberValue::Setter { .. },
        ..
      }
      | Syntax::ForStmt { .. }
      | Syntax::ForInStmt { .. }
      | Syntax::ForOfStmt { .. }
      | Syntax::FunctionDecl { .. }
      | Syntax::FunctionExpr { .. } => {
        self.restore_scope();
      }
      _ => {}
    };
    match node.stx.as_ref() {
      Syntax::CatchBlock { .. }
      | Syntax::FunctionSignature { .. }
      | Syntax::ImportStmt { .. }
      | Syntax::VarDecl { .. } => {
        self.restore_pattern_action();
      }
      _ => {}
    };
  }
}
