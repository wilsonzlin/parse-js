use std::collections::HashSet;

use serde_json::{json, Value};

use crate::{ast::{
    ArrayElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue, ExportNames,
    ForInOfStmtHeaderLhs, ForStmtHeader, ForThreeInit, LiteralTemplatePart, NodeId, NodeMap,
    ObjectMemberType, Syntax,
}, symbol::{ScopeMap, ScopeId}};

struct Visitor<'s> {
  node_map: &'s NodeMap,
  scope_map: &'s ScopeMap,
  visited_scopes: HashSet<ScopeId>,
}

impl <'s> Visitor<'s> {
  fn visit_class_or_object_member_key(&mut self, key: &ClassOrObjectMemberKey) -> Value {
    match key {
        ClassOrObjectMemberKey::Direct(r) => json!(r.as_str().to_string()),
        ClassOrObjectMemberKey::Computed(e) => self.visit_node(*e),
    }
  }

  fn visit_class_or_object_member_value(&mut self, value: &ClassOrObjectMemberValue) -> Value {
    match value {
        ClassOrObjectMemberValue::Getter { body } => json!({
            "getter": json!({
                "body": self.visit_node(*body),
            }),
        }),
        ClassOrObjectMemberValue::Method {
            is_async,
            generator,
            signature,
            body,
        } => json!({
            "method": json!({
                "async": is_async,
                "generator": generator,
                "signature": self.visit_node(*signature),
                "body": self.visit_node(*body),
            }),
        }),
        ClassOrObjectMemberValue::Property { initializer } => json!({
            "property": json!({
                "initializer": initializer.map(|n| self.visit_node(n)),
            }),
        }),
        ClassOrObjectMemberValue::Setter { parameter, body } => json!({
            "setter": json!({
                "parameter": self.visit_node(*parameter),
                "body": self.visit_node(*body),
            }),
        }),
    }
  }

  fn visit_node(&mut self, n: NodeId) -> Value {
    // Visit scope before visiting child nodes, as otherwise scope may be associated with misleading nested node.
    let scope_id = self.node_map[n].scope();
    let mut vars = Vec::new();
    if self.visited_scopes.insert(scope_id) {
      // TODO Use collect_into once stabilised.
      for ident in self.scope_map[scope_id].iter_names() {
        vars.push(Value::String(ident.as_str().to_string()));
      };
    };

    let mut serialized = match self.node_map[n].stx() {
        Syntax::IdentifierPattern { name } => json!({
            "$t": "IdentifierPattern",
            "name": name.as_str().to_string(),
        }),
        Syntax::ArrayPattern { elements, rest } => json!({
            "$t": "ArrayPattern",
            "elements": elements.iter().map(|e| e.as_ref().map(|e| json!({
                "target": self.visit_node(e.target),
                "default_value": e.default_value.map(|n| self.visit_node(n)),
            }))).collect::<Vec<_>>(),
            "rest": rest.map(|n| self.visit_node(n)),
        }),
        Syntax::ObjectPattern { properties, rest } => json!({
            "$t": "ObjectPattern",
            "properties": properties.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
            "rest": rest.map(|n| self.visit_node(n)),
        }),
        Syntax::ClassOrFunctionName { name } => json!({
            "$t": "FunctionName",
            "name": name.as_str().to_string(),
        }),
        Syntax::FunctionSignature { parameters } => json!({
            "$t": "FunctionSignature",
            "parameters": parameters.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
        }),
        Syntax::ClassDecl {
            name,
            extends,
            members,
        } => json!({
            "$t": "ClassDecl",
            "name": name.map(|n| self.visit_node(n)),
            "extends": extends.map(|n| self.visit_node(n)),
            "members": members.iter().map(|mem| json!({
                "static": mem.statik,
                "key": self.visit_class_or_object_member_key(&mem.key),
                "value": self.visit_class_or_object_member_value(&mem.value),
            })).collect::<Vec<_>>(),
        }),
        Syntax::ClassExpr {
            parenthesised,
            name,
            extends,
            members,
        } => json!({
            "$t": "ClassExpr",
            "parenthesised": parenthesised,
            "name": name.map(|n| self.visit_node(n)),
            "extends": extends.map(|n| self.visit_node(n)),
            "members": members.iter().map(|mem| json!({
                "static": mem.statik,
                "key": self.visit_class_or_object_member_key(&mem.key),
                "value": self.visit_class_or_object_member_value(&mem.value),
            })).collect::<Vec<_>>(),
        }),
        Syntax::FunctionDecl {
            is_async,
            generator,
            name,
            signature,
            body,
        } => json!({
            "$t": "FunctionDecl",
            "async": is_async,
            "generator": generator,
            "name": name.map(|n| self.visit_node(n)),
            "signature": self.visit_node(*signature),
            "body": self.visit_node(*body),
        }),
        Syntax::ParamDecl {
            rest,
            pattern,
            default_value,
        } => json!({
            "$t": "ParamDecl",
            "rest": rest,
            "pattern": self.visit_node(*pattern),
            "default_value": default_value.map(|n| self.visit_node(n)),
        }),
        Syntax::VarDecl { mode, declarators } => json!({
            "$t": "VarDecl",
            "mode": mode,
            "declarators": declarators.iter().map(|d| json!({
                "pattern": self.visit_node(d.pattern),
                "initializer": d.initializer.map(|n| self.visit_node(n)),
            })).collect::<Vec<_>>(),
        }),
        Syntax::ArrowFunctionExpr {
            parenthesised,
            is_async,
            signature,
            body,
        } => json!({
            "$t": "ArrowFunctionExpr",
            "parenthesised": parenthesised,
            "async": is_async,
            "signature": self.visit_node(*signature),
            "body": self.visit_node(*body),
        }),
        Syntax::BinaryExpr {
            parenthesised,
            operator,
            left,
            right,
        } => json!({
            "$t": "BinaryExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "left": self.visit_node(*left),
            "right": self.visit_node(*right),
        }),
        Syntax::CallExpr {
            parenthesised,
            optional_chaining,
            callee,
            arguments,
        } => json!({
            "$t": "CallExpr",
            "optional_chaining": optional_chaining,
            "parenthesised": parenthesised,
            "callee": self.visit_node(*callee),
            "arguments": arguments.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
        }),
        Syntax::ConditionalExpr {
            parenthesised,
            test,
            consequent,
            alternate,
        } => json!({
            "$t": "ConditionalExpr",
            "parenthesised": parenthesised,
            "test": self.visit_node(*test),
            "consequent": self.visit_node(*consequent),
            "alternate": self.visit_node(*alternate),
        }),
        Syntax::ComputedMemberExpr {
            optional_chaining,
            object,
            member,
        } => json!({
            "$t": "ComputedMemberExpr",
            "optional_chaining": optional_chaining,
            "object": self.visit_node(*object),
            "member": self.visit_node(*member),
        }),
        Syntax::FunctionExpr {
            parenthesised,
            is_async,
            generator,
            name,
            signature,
            body,
        } => json!({
            "$t": "FunctionExpr",
            "parenthesised": parenthesised,
            "async": is_async,
            "generator": generator,
            "name": name.as_ref().map(|n| self.visit_node(*n)),
            "signature": self.visit_node(*signature),
            "body": self.visit_node(*body),
        }),
        Syntax::IdentifierExpr { name } => json!({
            "$t": "IdentifierExpr",
            "name": name.as_str().to_string(),
        }),
        Syntax::ImportExpr { module } => json!({
            "$t": "ImportExpr",
           "module": self.visit_node(*module),
        }),
        Syntax::ImportMeta {} => json!({
            "$t": "ImportMeta",
        }),
        Syntax::JsxAttribute { name, value } => json!({
            "$t": "JsxAttribute",
            "name": self.visit_node(*name),
            "value": value.map(|n| self.visit_node(n)),
        }),
        Syntax::JsxName { namespace, name } => json!({
            "$t": "JsxName",
            "namespace": namespace.as_ref().map(|n| n.as_str().to_string()),
            "name": name.as_str().to_string(),
        }),
        Syntax::JsxMember { base, path } => json!({
            "$t": "JsxMember",
            "base": base.as_str().to_string(),
            "path": path.iter().map(|c| c.as_str().to_string()).collect::<Vec<_>>(),
        }),
        Syntax::JsxExpressionContainer { value } => json!({
            "$t": "JsxExpressionContainer",
            "value": self.visit_node(*value),
        }),
        Syntax::JsxSpreadAttribute { value } => json!({
            "$t": "JsxSpreadAttribute",
            "value": self.visit_node(*value),
        }),
        Syntax::JsxText { value } => json!({
            "$t": "JsxText",
            "value": value.as_str().to_string(),
        }),
        Syntax::JsxElement {
            name,
            attributes,
            children,
        } => json!({
            "$t": "JsxElement",
            "name": name.map(|n| self.visit_node(n)),
            "attributes": attributes.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
            "children": children.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
        }),
        Syntax::LiteralArrayExpr { elements } => json!({
            "$t": "LiteralArrayExpr",
            "elements": elements.iter().map(|e| match e {
                ArrayElement::Single(e) => json!({
                    "single": self.visit_node(*e),
                }),
                ArrayElement::Rest(e) => json!({
                    "rest": self.visit_node(*e),
                }),
                ArrayElement::Empty => Value::Null,
            }).collect::<Vec<_>>(),
        }),
        Syntax::LiteralBigIntExpr { value } => json!({
            "$t": "LiteralBigIntExpr",
            "value": value,
        }),
        Syntax::LiteralBooleanExpr { value } => json!({
            "$t": "LiteralBooleanExpr",
            "value": value,
        }),
        Syntax::LiteralNull {} => json!({
            "$t": "LiteralNull",
        }),
        Syntax::LiteralNumberExpr { value } => json!({
            "$t": "LiteralNumberExpr",
            "value": value.0,
        }),
        Syntax::LiteralObjectExpr { members } => json!({
            "$t": "LiteralObjectExpr",
            "members": members.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
        }),
        Syntax::LiteralRegexExpr {} => json!({
            "$t": "LiteralRegexExpr",
        }),
        Syntax::LiteralStringExpr { value } => json!({
            "$t": "LiteralStringExpr",
            "value": value.as_str().to_string(),
        }),
        Syntax::LiteralTemplateExpr { parts } => json!({
            "$t": "LiteralTemplateExpr",
            "parts": parts.iter().map(|p| match p {
              LiteralTemplatePart::String(string) => json!({
                  "string": string.as_str().to_string(),
              }),
              LiteralTemplatePart::Substitution(sub) => json!({
                  "substitution": self.visit_node(*sub),
              }),
            }).collect::<Vec<_>>(),
        }),
        Syntax::LiteralUndefined {} => json!({
            "$t": "LiteralUndefined",
        }),
        Syntax::ThisExpr {} => json!({
            "$t": "ThisExpr",
        }),
        Syntax::UnaryExpr {
            parenthesised,
            operator,
            argument,
        } => json!({
            "$t": "UnaryExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "argument": self.visit_node(*argument),
        }),
        Syntax::UnaryPostfixExpr {
            parenthesised,
            operator,
            argument,
        } => json!({
            "$t": "UnaryPostfixExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "argument": self.visit_node(*argument),
        }),
        Syntax::BlockStmt { body } => json!({
            "$t": "BlockStmt",
            "body": body.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
        }),
        Syntax::BreakStmt { label } => json!({
            "$t": "BreakStmt",
            "label": label.as_ref().map(|n| n.as_str().to_string()),
        }),
        Syntax::ContinueStmt { label } => json!({
            "$t": "ContinueStmt",
            "label": label.as_ref().map(|n| n.as_str().to_string()),
        }),
        Syntax::DebuggerStmt {} => json!({
            "$t": "DebuggerStmt",
        }),
        Syntax::DoWhileStmt { condition, body } => json!({
            "$t": "DoWhileStmt",
            "condition": self.visit_node(*condition),
            "body": self.visit_node(*body)
        }),
        Syntax::EmptyStmt {} => json!({
            "$t": "EmptyStmt",
        }),
        Syntax::ExportDeclStmt {
            declaration,
            default,
        } => json!({
            "$t": "ExportDeclStmt",
            "declaration": self.visit_node(*declaration),
            "default": default,
        }),
        Syntax::ExportDefaultExprStmt { expression } => json!({
            "$t": "ExportDefaultStmt",
            "expression": self.visit_node(*expression),
        }),
        Syntax::ExportListStmt { names, from } => json!({
            "$t": "ExportListStmt",
            "names": match names {
              ExportNames::All(p) => json!({
                  "all": p.map(|n| self.visit_node(n)),
              }),
              ExportNames::Specific(names) => json!({
                  "specific": names.iter().map(|n| json!({
                      "target": n.target.as_str().to_string(),
                      "alias": self.visit_node(n.alias),
                  })).collect::<Vec<_>>(),
              })
            },
            "from": from,
        }),
        Syntax::ExpressionStmt { expression } => json!({
            "$t": "ExpressionStmt",
            "expression": self.visit_node(*expression),
        }),
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        } => json!({
            "$t": "IfStmt",
            "test": self.visit_node(*test),
            "consequent": self.visit_node(*consequent),
            "alternate": alternate.map(|n| self.visit_node(n)),
        }),
        Syntax::ImportStmt {
            default,
            names,
            module,
        } => todo!(),
        Syntax::ForStmt { header, body } => json!({
            "$t": "ForStmt",
            "header": match header {
                ForStmtHeader::Three { init, condition, post } => json!({
                    "three": json!({
                        "init": match init {
                            ForThreeInit::None => Value::Null,
                            ForThreeInit::Expression(e) => json!({
                                "expression": self.visit_node(*e),
                            }),
                            ForThreeInit::Declaration(e) => json!({
                                "declaration": self.visit_node(*e),
                            }),
                        },
                        "condition": condition.map(|n| self.visit_node(n)),
                        "post": post.map(|n| self.visit_node(n)),
                    }),
                }),
                ForStmtHeader::InOf { of, lhs, rhs } => json!({
                    "inOf": json!({
                        "of": of,
                        "lhs": match lhs {
                            ForInOfStmtHeaderLhs::Declaration(decl) => json!({
                                "declaration": self.visit_node(*decl),
                            }),
                            ForInOfStmtHeaderLhs::Pattern(pat) => json!({
                                "pattern": self.visit_node(*pat),
                            }),
                        },
                        "rhs": self.visit_node(*rhs),
                    }),
                }),
            },
            "body": self.visit_node(*body),
        }),
        Syntax::LabelStmt { name, statement } => json!({
            "$t": "LabelStmt",
            "name": name.as_str().to_string(),
            "statement": self.visit_node(*statement),
        }),
        Syntax::ReturnStmt { value } => json!({
            "$t": "ReturnStmt",
            "value": value.map(|n| self.visit_node(n)) }),
        Syntax::SwitchStmt { test, branches } => json!({
            "$t": "SwitchStmt",
            "test": self.visit_node(*test),
            "branches": branches.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
        }),
        Syntax::ThrowStmt { value } => json!({
            "$t": "ThrowStmt",
            "value": self.visit_node(*value),
        }),
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        } => json!({
            "$t": "TryStmt",
            "wrapped": self.visit_node(*wrapped),
            "catch": catch.map(|n| self.visit_node(n)),
            "finally": finally.map(|n| self.visit_node(n)),
        }),
        Syntax::VarStmt { declaration } => json!({
            "$t": "VarStmt", "declaration": self.visit_node(*declaration) }),
        Syntax::WhileStmt { condition, body } => json!({
            "$t": "WhileStmt",
            "condition": self.visit_node(*condition),
            "body": self.visit_node(*body),
        }),
        Syntax::TopLevel { body } => json!({
            "$t": "TopLevel",
            "body": body.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>() }),
        Syntax::CatchBlock { parameter, body } => json!({
            "$t": "CatchBlock",
            "parameter": parameter.map(|n| self.visit_node(n)),
            "body": self.visit_node(*body),
        }),
        Syntax::SwitchBranch { case, body } => json!({
            "$t": "SwitchBranch",
            "case": case.map(|n| self.visit_node(n)),
            "body": body.iter().map(|n| self.visit_node(*n)).collect::<Vec<_>>(),
        }),
        Syntax::ObjectMember { typ } => json!({
            "$t": "ObjectMember",
            "typ": match typ {
                ObjectMemberType::Rest { value } => json!({
                    "rest": self.visit_node(*value),
                }),
                ObjectMemberType::Shorthand { name } => json!({
                    "shorthand": json!(name.as_str().to_string()),
                }),
                ObjectMemberType::Valued { key, value } => json!({
                    "valued": json!({
                        "key": self.visit_class_or_object_member_key(key),
                        "value": self.visit_class_or_object_member_value(value),
                    }),
                }),
            },
        }),
        Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
        } => json!({
            "$t": "ObjectPatternProperty",
            "key": self.visit_class_or_object_member_key(key),
            "target": target.map(|n| self.visit_node(n)),
            "default": default_value.map(|n| self.visit_node(n)),
        }),
        Syntax::MemberExpr {
            parenthesised,
            optional_chaining,
            left,
            right,
        } => json!({
            "$t": "MemberExpr",
            "parenthesised": parenthesised,
            "optional_chaining": optional_chaining,
            "left": self.visit_node(*left),
            "right": right.as_str().to_string(),
        }),
        Syntax::CallArg { spread, value } => json!({
            "$t": "CallArg",
            "spread": spread,
            "value": self.visit_node(*value),
        }),
        Syntax::SuperExpr {} => json!({
            "$t": "SuperExpr",
        }),
    };

    if !vars.is_empty() {
      serialized["$vars"] = Value::Array(vars);
    };

    serialized
  }
}

pub fn serialize_ast(
  scope_map: &ScopeMap,
  node_map: &NodeMap,
  top_level_node_id: NodeId,
) -> Value {
    let mut visitor = Visitor {
      node_map,
      scope_map,
      visited_scopes: HashSet::new(),
    };
    visitor.visit_node(top_level_node_id)
}
