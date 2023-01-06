use crate::ast::{
    ArrayElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue, ExportNames,
    ForInOfStmtHeaderLhs, ForStmtHeader, ForThreeInit, LiteralTemplatePart, NodeId, NodeMap,
    ObjectMemberType, Syntax,
};

pub struct JourneyControls {
    skip: bool,
}

impl JourneyControls {
    pub fn skip(&mut self) -> () {
        self.skip = true;
    }
}

// Note that the NodeMap must be provided as a separate immutable reference parameter, as accessing it from &mut self will cause Rust to appropriately detect it as borrowing as mutable (visiting the child with &mut self) while borrowed as immutable (calling the visitor on each property of the parent node).
pub trait Visitor {
    fn on_syntax(&mut self, parent_node: NodeId, node: NodeId, ctl: &mut JourneyControls) -> ();

    fn visit_class_or_object_key(
        &mut self,
        node_map: &NodeMap,
        parent: NodeId,
        key: &ClassOrObjectMemberKey,
    ) -> () {
        match key {
            ClassOrObjectMemberKey::Direct(_) => {}
            ClassOrObjectMemberKey::Computed(key) => self.visit(node_map, parent, *key),
        };
    }

    fn visit_class_or_object_value(
        &mut self,
        node_map: &NodeMap,
        parent: NodeId,
        value: &ClassOrObjectMemberValue,
    ) -> () {
        match value {
            ClassOrObjectMemberValue::Getter { body } => self.visit(node_map, parent, *body),
            ClassOrObjectMemberValue::Method {
                signature, body, ..
            } => {
                self.visit(node_map, parent, *signature);
                self.visit(node_map, parent, *body);
            }
            ClassOrObjectMemberValue::Property { initializer } => {
                if let Some(initializer) = initializer {
                    self.visit(node_map, parent, *initializer);
                };
            }
            ClassOrObjectMemberValue::Setter { body, parameter } => {
                self.visit(node_map, parent, *parameter);
                self.visit(node_map, parent, *body);
            }
        }
    }

    fn visit_top_level(&mut self, node_map: &NodeMap, top_level_node_id: NodeId) -> () {
        match node_map[top_level_node_id].stx() {
            Syntax::TopLevel { body } => {
                for stmt in body {
                    self.visit(node_map, top_level_node_id, *stmt);
                }
            }
            _ => panic!("not top level"),
        };
    }

    fn visit(&mut self, node_map: &NodeMap, parent: NodeId, n: NodeId) -> () {
        let mut ctl = JourneyControls { skip: false };
        self.on_syntax(parent, n, &mut ctl);
        if ctl.skip {
            return;
        };
        match node_map[n].stx() {
            Syntax::FunctionExpr {
                name,
                signature,
                body,
                ..
            } => {
                if let Some(name) = name {
                    self.visit(node_map, n, *name);
                };
                self.visit(node_map, n, *signature);
                self.visit(node_map, n, *body);
            }
            Syntax::IdentifierPattern { .. }
            | Syntax::IdentifierExpr { .. }
            | Syntax::ClassOrFunctionName { .. } => {}
            Syntax::ArrayPattern { elements, rest } => {
                for elem in elements {
                    if let Some(elem) = elem {
                        self.visit(node_map, n, elem.target);
                    };
                }
                if let Some(rest) = rest {
                    self.visit(node_map, n, *rest);
                };
            }
            Syntax::ArrowFunctionExpr {
                signature, body, ..
            } => {
                self.visit(node_map, n, *signature);
                self.visit(node_map, n, *body);
            }
            Syntax::BinaryExpr { left, right, .. } => {
                self.visit(node_map, n, *left);
                self.visit(node_map, n, *right);
            }
            Syntax::BlockStmt { body } => {
                for stmt in body {
                    self.visit(node_map, n, *stmt);
                }
            }
            Syntax::BreakStmt { .. } => {}
            Syntax::CallExpr {
                callee, arguments, ..
            } => {
                self.visit(node_map, n, *callee);
                for arg in arguments {
                    self.visit(node_map, n, *arg);
                }
            }
            Syntax::CatchBlock { parameter, body } => {
                if let Some(param) = parameter {
                    self.visit(node_map, n, *param);
                }
                self.visit(node_map, n, *body);
            }
            Syntax::ClassDecl {
                name,
                extends,
                members,
            } => {
                if let Some(name) = name {
                    self.visit(node_map, n, *name);
                }
                if let Some(extends) = extends {
                    self.visit(node_map, n, *extends);
                };
                for member in members {
                    self.visit_class_or_object_key(node_map, n, &member.key);
                    self.visit_class_or_object_value(node_map, n, &member.value);
                }
            }
            Syntax::ClassExpr {
                name,
                extends,
                members,
                ..
            } => {
                if let Some(name) = name {
                    self.visit(node_map, n, *name);
                }
                if let Some(extends) = extends {
                    self.visit(node_map, n, *extends);
                };
                for member in members {
                    self.visit_class_or_object_key(node_map, n, &member.key);
                    self.visit_class_or_object_value(node_map, n, &member.value);
                }
            }
            Syntax::ComputedMemberExpr { object, member, .. } => {
                self.visit(node_map, n, *object);
                self.visit(node_map, n, *member);
            }
            Syntax::ConditionalExpr {
                test,
                consequent,
                alternate,
                ..
            } => {
                self.visit(node_map, n, *test);
                self.visit(node_map, n, *consequent);
                self.visit(node_map, n, *alternate);
            }
            Syntax::ContinueStmt { .. } => {}
            Syntax::DebuggerStmt {} => {}
            Syntax::DoWhileStmt { condition, body } => {
                self.visit(node_map, n, *body);
                self.visit(node_map, n, *condition);
            }
            Syntax::EmptyStmt {} => {}
            Syntax::ExportDeclStmt { declaration, .. } => {
                self.visit(node_map, n, *declaration);
            }
            Syntax::ExportDefaultExprStmt { expression } => self.visit(node_map, n, *expression),
            Syntax::ExportListStmt { names, .. } => match names {
                ExportNames::All(alias) => {
                    if let Some(alias) = alias {
                        self.visit(node_map, n, *alias);
                    }
                }
                ExportNames::Specific(imports) => {
                    for imp in imports {
                        self.visit(node_map, n, imp.alias);
                    }
                }
            },
            Syntax::ExpressionStmt { expression } => {
                self.visit(node_map, n, *expression);
            }
            Syntax::ForStmt { header, body } => {
                match header {
                    ForStmtHeader::Three {
                        init,
                        condition,
                        post,
                    } => {
                        match init {
                            ForThreeInit::None => {}
                            ForThreeInit::Expression(expr) => self.visit(node_map, n, *expr),
                            ForThreeInit::Declaration(decl) => self.visit(node_map, n, *decl),
                        };
                        if let Some(condition) = condition {
                            self.visit(node_map, n, *condition);
                        }
                        if let Some(post) = post {
                            self.visit(node_map, n, *post);
                        }
                    }
                    ForStmtHeader::InOf { lhs, rhs, .. } => {
                        match lhs {
                            ForInOfStmtHeaderLhs::Declaration(decl) => {
                                self.visit(node_map, n, *decl)
                            }
                            ForInOfStmtHeaderLhs::Pattern(pat) => self.visit(node_map, n, *pat),
                        }
                        self.visit(node_map, n, *rhs);
                    }
                };
                self.visit(node_map, n, *body);
            }
            Syntax::FunctionDecl {
                name,
                signature,
                body,
                ..
            } => {
                if let Some(name) = name {
                    self.visit(node_map, n, *name);
                }
                self.visit(node_map, n, *signature);
                self.visit(node_map, n, *body);
            }
            Syntax::FunctionSignature { parameters } => {
                for param in parameters {
                    self.visit(node_map, n, *param);
                }
            }
            Syntax::IfStmt {
                test,
                consequent,
                alternate,
            } => {
                self.visit(node_map, n, *test);
                self.visit(node_map, n, *consequent);
                if let Some(alternate) = alternate {
                    self.visit(node_map, n, *alternate);
                };
            }
            Syntax::ImportExpr { module } => self.visit(node_map, n, *module),
            Syntax::ImportMeta {} => {}
            Syntax::ImportStmt { default, names, .. } => {
                if let Some(default) = default {
                    self.visit(node_map, n, *default);
                };
                for name in names {
                    match name {
                        ExportNames::All(alias) => {
                            if let Some(alias) = alias {
                                self.visit(node_map, n, *alias);
                            }
                        }
                        ExportNames::Specific(names) => {
                            for name in names {
                                self.visit(node_map, n, name.alias);
                            }
                        }
                    }
                }
            }
            Syntax::JsxAttribute { name, value } => {
                self.visit(node_map, n, *name);
                if let Some(value) = value {
                    self.visit(node_map, n, *value);
                };
            }
            Syntax::JsxElement {
                attributes,
                children,
                name,
            } => {
                if let Some(name) = name {
                    self.visit(node_map, n, *name);
                }
                for attr in attributes {
                    self.visit(node_map, n, *attr);
                }
                for child in children {
                    self.visit(node_map, n, *child);
                }
            }
            Syntax::JsxExpressionContainer { value } => {
                self.visit(node_map, n, *value);
            }
            Syntax::JsxMember { .. } => {}
            Syntax::JsxName { .. } => {}
            Syntax::JsxSpreadAttribute { value } => {
                self.visit(node_map, n, *value);
            }
            Syntax::JsxText { .. } => {}
            Syntax::LiteralArrayExpr { elements } => {
                for elem in elements {
                    match elem {
                        ArrayElement::Single(elem) => self.visit(node_map, n, *elem),
                        ArrayElement::Rest(elem) => self.visit(node_map, n, *elem),
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
                    self.visit(node_map, n, *member);
                }
            }
            Syntax::LiteralRegexExpr {} => {}
            Syntax::LiteralStringExpr { .. } => {}
            Syntax::LiteralTemplateExpr { parts } => {
                for part in parts {
                    match part {
                        LiteralTemplatePart::Substitution(expr) => self.visit(node_map, n, *expr),
                        LiteralTemplatePart::String(_) => {}
                    }
                }
            }
            Syntax::LiteralUndefined {} => {}
            Syntax::ObjectPattern { properties, rest } => {
                for prop in properties {
                    self.visit(node_map, n, *prop);
                }
                if let Some(rest) = rest {
                    self.visit(node_map, n, *rest);
                }
            }
            Syntax::ObjectPatternProperty {
                key,
                target,
                default_value,
            } => {
                match key {
                    ClassOrObjectMemberKey::Direct(..) => {}
                    ClassOrObjectMemberKey::Computed(key) => self.visit(node_map, n, *key),
                };
                if let Some(target) = target {
                    self.visit(node_map, n, *target);
                }
                if let Some(value) = default_value {
                    self.visit(node_map, n, *value);
                }
            }
            Syntax::ParamDecl {
                pattern,
                default_value,
                ..
            } => {
                self.visit(node_map, n, *pattern);
                if let Some(value) = default_value {
                    self.visit(node_map, n, *value);
                }
            }
            Syntax::ReturnStmt { value } => {
                if let Some(value) = value {
                    self.visit(node_map, n, *value);
                }
            }
            Syntax::SwitchBranch { case, body } => {
                if let Some(value) = case {
                    self.visit(node_map, n, *value);
                }
                for stmt in body {
                    self.visit(node_map, n, *stmt);
                }
            }
            Syntax::SwitchStmt { test, branches } => {
                self.visit(node_map, n, *test);
                for branch in branches {
                    self.visit(node_map, n, *branch);
                }
            }
            Syntax::ThisExpr {} => {}
            Syntax::ThrowStmt { value } => {
                self.visit(node_map, n, *value);
            }
            Syntax::TopLevel { .. } => unreachable!(),
            Syntax::TryStmt {
                wrapped,
                catch,
                finally,
            } => {
                self.visit(node_map, n, *wrapped);
                if let Some(catch) = catch {
                    self.visit(node_map, n, *catch);
                }
                if let Some(finally) = finally {
                    self.visit(node_map, n, *finally);
                }
            }
            Syntax::UnaryExpr { argument, .. } => {
                self.visit(node_map, n, *argument);
            }
            Syntax::UnaryPostfixExpr { argument, .. } => {
                self.visit(node_map, n, *argument);
            }
            Syntax::VarDecl { declarators, .. } => {
                for decl in declarators {
                    self.visit(node_map, n, decl.pattern);
                    if let Some(init) = decl.initializer {
                        self.visit(node_map, n, init);
                    }
                }
            }
            Syntax::VarStmt { declaration } => {
                self.visit(node_map, n, *declaration);
            }
            Syntax::WhileStmt { condition, body } => {
                self.visit(node_map, n, *condition);
                self.visit(node_map, n, *body);
            }
            Syntax::ObjectMember { typ } => {
                match typ {
                    ObjectMemberType::Valued { key, value } => {
                        self.visit_class_or_object_key(node_map, n, key);
                        self.visit_class_or_object_value(node_map, n, value);
                    }
                    ObjectMemberType::Shorthand { .. } => {}
                    ObjectMemberType::Rest { value } => {
                        self.visit(node_map, n, *value);
                    }
                };
            }
            Syntax::MemberExpr { left, .. } => {
                self.visit(node_map, n, *left);
            }
            Syntax::LabelStmt { statement, .. } => {
                self.visit(node_map, n, *statement);
            }
            Syntax::CallArg { value, .. } => {
                self.visit(node_map, n, *value);
            }
            Syntax::SuperExpr {} => {}
        };
    }
}
