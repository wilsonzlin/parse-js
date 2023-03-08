use crate::ast::ArrayElement;
use crate::ast::ClassOrObjectMemberKey;
use crate::ast::ClassOrObjectMemberValue;
use crate::ast::ExportNames;
use crate::ast::ForInOfStmtHeaderLhs;
use crate::ast::ForStmtHeader;
use crate::ast::ForThreeInit;
use crate::ast::LiteralTemplatePart;
use crate::ast::NodeData;
use crate::ast::ObjectMemberType;
use crate::ast::Syntax;
use crate::operator::OperatorName;

pub struct JourneyControls {
  skip: bool,
}

impl JourneyControls {
  pub fn skip(&mut self) -> () {
    self.skip = true;
  }
}

// Don't use `Node<'a>` as that requires reference to live for entire `'a`.
// Nodes must be visited in execution order. This is helpful for many uses.
pub trait Visitor<'a> {
  fn on_syntax_down(&mut self, node: &mut NodeData<'a>, ctl: &mut JourneyControls) -> () {}

  fn on_syntax_up(&mut self, node: &mut NodeData<'a>) -> () {}

  fn visit_class_or_object_key(&mut self, key: &mut ClassOrObjectMemberKey<'a>) -> () {
    match key {
      ClassOrObjectMemberKey::Direct(_) => {}
      ClassOrObjectMemberKey::Computed(key) => self.visit(*key),
    };
  }

  fn visit_class_or_object_value(&mut self, value: &mut ClassOrObjectMemberValue<'a>) -> () {
    match value {
      ClassOrObjectMemberValue::Getter { body } => self.visit(*body),
      ClassOrObjectMemberValue::Method {
        signature, body, ..
      } => {
        self.visit(*signature);
        self.visit(*body);
      }
      ClassOrObjectMemberValue::Property { initializer } => {
        if let Some(initializer) = initializer {
          self.visit(*initializer);
        };
      }
      ClassOrObjectMemberValue::Setter { body, parameter } => {
        self.visit(*parameter);
        self.visit(*body);
      }
    }
  }

  fn visit(&mut self, n: &mut NodeData<'a>) -> () {
    let mut cur_stx_type = core::mem::discriminant(&n.stx);
    loop {
      let mut ctl = JourneyControls { skip: false };
      self.on_syntax_down(n, &mut ctl);
      if ctl.skip {
        return;
      };
      let new_stx_type = core::mem::discriminant(&n.stx);
      if cur_stx_type == new_stx_type {
        break;
      };
      cur_stx_type = new_stx_type;
    }

    match &mut n.stx {
      Syntax::TopLevel { body } => {
        for stmt in body {
          self.visit(stmt);
        }
      }
      Syntax::FunctionExpr {
        name,
        signature,
        body,
        ..
      } => {
        if let Some(name) = name {
          self.visit(*name);
        };
        self.visit(*signature);
        self.visit(*body);
      }
      Syntax::IdentifierPattern { .. }
      | Syntax::IdentifierExpr { .. }
      | Syntax::ClassOrFunctionName { .. } => {}
      Syntax::ArrayPattern { elements, rest } => {
        for elem in elements {
          if let Some(elem) = elem {
            self.visit(elem.target);
          };
        }
        if let Some(rest) = rest {
          self.visit(*rest);
        };
      }
      Syntax::ArrowFunctionExpr {
        signature, body, ..
      } => {
        self.visit(*signature);
        self.visit(*body);
      }
      Syntax::BinaryExpr {
        left,
        right,
        operator,
        ..
      } => {
        if operator.is_assignment()
          && match left.stx {
            Syntax::ArrayPattern { .. }
            | Syntax::ObjectPattern { .. }
            | Syntax::IdentifierPattern { .. } => true,
            _ => false,
          }
        {
          self.visit(*right);
          self.visit(*left);
        } else {
          self.visit(*left);
          self.visit(*right);
        };
      }
      Syntax::BlockStmt { body } => {
        for stmt in body {
          self.visit(*stmt);
        }
      }
      Syntax::BreakStmt { .. } => {}
      Syntax::CallExpr {
        callee, arguments, ..
      } => {
        for arg in arguments {
          self.visit(*arg);
        }
        self.visit(*callee);
      }
      Syntax::CatchBlock { parameter, body } => {
        if let Some(param) = parameter {
          self.visit(*param);
        }
        self.visit(*body);
      }
      Syntax::ClassDecl {
        name,
        extends,
        members,
        ..
      } => {
        if let Some(name) = name {
          self.visit(*name);
        }
        if let Some(extends) = extends {
          self.visit(*extends);
        };
        for member in members {
          self.visit_class_or_object_key(&mut member.key);
          self.visit_class_or_object_value(&mut member.value);
        }
      }
      Syntax::ClassExpr {
        name,
        extends,
        members,
        ..
      } => {
        if let Some(name) = name {
          self.visit(*name);
        }
        if let Some(extends) = extends {
          self.visit(*extends);
        };
        for member in members {
          self.visit_class_or_object_key(&mut member.key);
          self.visit_class_or_object_value(&mut member.value);
        }
      }
      Syntax::ComputedMemberExpr { object, member, .. } => {
        self.visit(*object);
        self.visit(*member);
      }
      Syntax::ConditionalExpr {
        test,
        consequent,
        alternate,
        ..
      } => {
        self.visit(*test);
        self.visit(*consequent);
        self.visit(*alternate);
      }
      Syntax::ContinueStmt { .. } => {}
      Syntax::DebuggerStmt {} => {}
      Syntax::DoWhileStmt { condition, body } => {
        self.visit(*body);
        self.visit(*condition);
      }
      Syntax::EmptyStmt {} => {}
      Syntax::ExportDefaultExprStmt { expression } => self.visit(*expression),
      Syntax::ExportListStmt { names, .. } => match names {
        ExportNames::All(alias) => {
          if let Some(alias) = alias {
            self.visit(*alias);
          }
        }
        ExportNames::Specific(imports) => {
          for imp in imports {
            self.visit(imp.alias);
          }
        }
      },
      Syntax::ExpressionStmt { expression } => {
        self.visit(*expression);
      }
      Syntax::ForStmt { header, body } => {
        match header {
          ForStmtHeader::Three {
            init, condition, ..
          } => {
            match init {
              ForThreeInit::None => {}
              ForThreeInit::Expression(expr) => self.visit(*expr),
              ForThreeInit::Declaration(decl) => self.visit(*decl),
            };
            if let Some(condition) = condition {
              self.visit(*condition);
            }
          }
          ForStmtHeader::InOf { lhs, rhs, .. } => {
            self.visit(*rhs);
            match lhs {
              ForInOfStmtHeaderLhs::Declaration(decl) => self.visit(*decl),
              ForInOfStmtHeaderLhs::Pattern(pat) => self.visit(*pat),
            }
          }
        };
        self.visit(*body);
        if let ForStmtHeader::Three {
          post: Some(post), ..
        } = header
        {
          self.visit(*post);
        };
      }
      Syntax::FunctionDecl {
        name,
        signature,
        body,
        ..
      } => {
        if let Some(name) = name {
          self.visit(*name);
        }
        self.visit(*signature);
        self.visit(*body);
      }
      Syntax::FunctionSignature { parameters } => {
        for param in parameters {
          self.visit(*param);
        }
      }
      Syntax::IfStmt {
        test,
        consequent,
        alternate,
      } => {
        self.visit(*test);
        self.visit(*consequent);
        if let Some(alternate) = alternate {
          self.visit(*alternate);
        };
      }
      Syntax::ImportExpr { module } => self.visit(*module),
      Syntax::ImportMeta {} => {}
      Syntax::ImportStmt { default, names, .. } => {
        if let Some(default) = default {
          self.visit(*default);
        };
        for name in names {
          match name {
            ExportNames::All(alias) => {
              if let Some(alias) = alias {
                self.visit(*alias);
              }
            }
            ExportNames::Specific(names) => {
              for name in names {
                self.visit(name.alias);
              }
            }
          }
        }
      }
      Syntax::JsxAttribute { name, value } => {
        self.visit(*name);
        if let Some(value) = value {
          self.visit(*value);
        };
      }
      Syntax::JsxElement {
        attributes,
        children,
        name,
      } => {
        if let Some(name) = name {
          self.visit(*name);
        }
        for attr in attributes {
          self.visit(*attr);
        }
        for child in children {
          self.visit(*child);
        }
      }
      Syntax::JsxExpressionContainer { value } => {
        self.visit(*value);
      }
      Syntax::JsxMemberExpression { base, .. } => {
        self.visit(*base);
      }
      Syntax::JsxName { .. } => {}
      Syntax::JsxSpreadAttribute { value } => {
        self.visit(*value);
      }
      Syntax::JsxText { .. } => {}
      Syntax::LiteralArrayExpr { elements } => {
        for elem in elements {
          match elem {
            ArrayElement::Single(elem) => self.visit(*elem),
            ArrayElement::Rest(elem) => self.visit(*elem),
            ArrayElement::Empty => {}
          }
        }
      }
      Syntax::LiteralBigIntExpr { .. } => {}
      Syntax::LiteralBooleanExpr { .. } => {}
      Syntax::LiteralNull {} => {}
      Syntax::LiteralNumberExpr { .. } => {}
      Syntax::LiteralObjectExpr { members } => {
        for member in members {
          self.visit(*member);
        }
      }
      Syntax::LiteralRegexExpr {} => {}
      Syntax::LiteralStringExpr { .. } => {}
      Syntax::LiteralTemplateExpr { parts } => {
        for part in parts {
          match part {
            LiteralTemplatePart::Substitution(expr) => self.visit(*expr),
            LiteralTemplatePart::String(_) => {}
          }
        }
      }
      Syntax::ObjectPattern { properties, rest } => {
        for prop in properties {
          self.visit(*prop);
        }
        if let Some(rest) = rest {
          self.visit(*rest);
        }
      }
      Syntax::ObjectPatternProperty {
        key,
        target,
        default_value,
        ..
      } => {
        match key {
          ClassOrObjectMemberKey::Direct(..) => {}
          ClassOrObjectMemberKey::Computed(key) => self.visit(*key),
        };
        self.visit(*target);
        if let Some(value) = default_value {
          self.visit(*value);
        }
      }
      Syntax::ParamDecl {
        pattern,
        default_value,
        ..
      } => {
        self.visit(*pattern);
        if let Some(value) = default_value {
          self.visit(*value);
        }
      }
      Syntax::ReturnStmt { value } => {
        if let Some(value) = value {
          self.visit(*value);
        }
      }
      Syntax::SwitchBranch { case, body } => {
        if let Some(value) = case {
          self.visit(*value);
        }
        for stmt in body {
          self.visit(*stmt);
        }
      }
      Syntax::SwitchStmt { test, branches } => {
        self.visit(*test);
        for branch in branches {
          self.visit(*branch);
        }
      }
      Syntax::ThisExpr {} => {}
      Syntax::ThrowStmt { value } => {
        self.visit(*value);
      }
      Syntax::TryStmt {
        wrapped,
        catch,
        finally,
      } => {
        self.visit(*wrapped);
        if let Some(catch) = catch {
          self.visit(*catch);
        }
        if let Some(finally) = finally {
          self.visit(*finally);
        }
      }
      Syntax::UnaryExpr { argument, .. } => {
        self.visit(*argument);
      }
      Syntax::UnaryPostfixExpr { argument, .. } => {
        self.visit(*argument);
      }
      Syntax::VarDecl { declarators, .. } => {
        for decl in declarators {
          self.visit(decl.pattern);
          if let Some(init) = &mut decl.initializer {
            self.visit(init);
          }
        }
      }
      Syntax::WhileStmt { condition, body } => {
        self.visit(*condition);
        self.visit(*body);
      }
      Syntax::ObjectMember { typ } => {
        match typ {
          ObjectMemberType::Valued { key, value } => {
            self.visit_class_or_object_key(key);
            self.visit_class_or_object_value(value);
          }
          ObjectMemberType::Shorthand { .. } => {}
          ObjectMemberType::Rest { value } => {
            self.visit(*value);
          }
        };
      }
      Syntax::MemberExpr { left, .. } => {
        self.visit(*left);
      }
      Syntax::LabelStmt { statement, .. } => {
        self.visit(*statement);
      }
      Syntax::CallArg { value, .. } => {
        self.visit(*value);
      }
      Syntax::SuperExpr {} => {}
      Syntax::_TakenNode {} => unreachable!(),
    };

    loop {
      self.on_syntax_up(n);
      let new_stx_type = core::mem::discriminant(&n.stx);
      if cur_stx_type == new_stx_type {
        break;
      };
      cur_stx_type = new_stx_type;
    }
  }
}
