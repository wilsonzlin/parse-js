use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::num::JsNumber;
use crate::operator::OperatorName;
use crate::session::Session;
use crate::session::SessionString;
use crate::session::SessionVec;
use crate::source::SourceRange;
use crate::symbol::Scope;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;

struct NodeData<'a> {
  loc: SourceRange<'a>,
  stx: Syntax<'a>,
  // For the purposes of disambiguation, the scope of a function or block is only set on its children and not itself. This is merely an arbitrary decision. For example, the scope created by a function is assigned to its signature nodes (and descendants e.g. default values), but not to the FunctionStmt itself. For a `for` loop, the scope created by it is assigned to its header nodes and descendants, but not to the ForStmt itself. For a block statement, the scope created by it is assigned to statements inside it, but not to the BlockStmt itself.
  scope: Scope<'a>,
}

// We use a newtype with RefCell instead of `&'a mut NodeData<'a>` because even though currently we don't reference a node from multiple places inside an AST, we do share Node references elsewhere (e.g. symbols, scopes). We still want mutability because otherwise we can't do mutations while traversing (important for many same-pass optimisations); even if we recreate nodes instead of updating a field, we still need to update the parent's reference (and without mutability, this would require rebuilding the entire tree). Typing `&'a mut NodeData<'a>` everywhere instead of `Node<'a>` is also too verbose and easy to mistype the lifetimes.
#[derive(Clone, Copy)]
pub struct Node<'a>(&'a RefCell<NodeData<'a>>);

// To prevent ambiguity and confusion, don't derive Eq, as there are multiple meanings of "equality" for nodes:
// - Exact identical instances, so two different nodes with the same syntax, location, and scope would still be different.
// - Same syntax, location, and scope.
// - Same syntax and location, but scope can be different.
// - Same syntax, but location and scope can be different.
impl<'a> Node<'a> {
  fn get<'b>(self) -> Ref<'b, NodeData<'a>> {
    self.0.borrow()
  }

  fn get_mut<'b>(self) -> RefMut<'b, NodeData<'a>> {
    self.0.borrow_mut()
  }

  pub fn new(
    session: &'a Session,
    scope: Scope<'a>,
    loc: SourceRange<'a>,
    stx: Syntax<'a>,
  ) -> Node<'a> {
    Node(
      session
        .mem
        .alloc(RefCell::new(NodeData { loc, stx, scope })),
    )
  }

  pub fn error(self, typ: SyntaxErrorType) -> SyntaxError<'a> {
    SyntaxError::from_loc(self.loc(), typ, None)
  }

  pub fn loc(self) -> SourceRange<'a> {
    self.get().loc
  }

  pub fn stx<'b>(self) -> Ref<'b, Syntax<'a>> {
    Ref::map(self.get(), |node| &node.stx)
  }

  pub fn stx_mut<'b>(self) -> RefMut<'b, Syntax<'a>> {
    RefMut::map(self.get_mut(), |node| &mut node.stx)
  }

  pub fn scope(self) -> Scope<'a> {
    self.get().scope
  }
}

#[cfg(feature = "serialize")]
impl<'a> serde::Serialize for Node<'a> {
  fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.stx().serialize(serializer)
  }
}

// These are for readability only, and do not increase type safety or define different structures.
type Declaration<'a> = Node<'a>;
type Expression<'a> = Node<'a>;
type Pattern<'a> = Node<'a>;
type Statement<'a> = Node<'a>;

#[derive(Eq, PartialEq, Clone, Copy)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum VarDeclMode {
  Const,
  Let,
  Var,
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ArrayElement<'a> {
  Single(Expression<'a>),
  Rest(Expression<'a>),
  Empty,
}

#[derive(Clone, Copy)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ClassOrObjectMemberKey<'a> {
  // Identifier, keyword, string, or number.
  Direct(SourceRange<'a>),
  Computed(Expression<'a>),
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ClassOrObjectMemberValue<'a> {
  Getter {
    body: Statement<'a>,
  },
  Method {
    is_async: bool,
    generator: bool,
    signature: Node<'a>,
    body: Statement<'a>,
  },
  Property {
    // Must be Some if object, as shorthands are covered by ObjectMemberType::Shorthand (and are initialised).
    initializer: Option<Expression<'a>>,
  },
  Setter {
    body: Statement<'a>,
    parameter: Pattern<'a>,
  },
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ClassMember<'a> {
  pub key: ClassOrObjectMemberKey<'a>,
  pub statik: bool,
  pub value: ClassOrObjectMemberValue<'a>,
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ObjectMemberType<'a> {
  Valued {
    key: ClassOrObjectMemberKey<'a>,
    value: ClassOrObjectMemberValue<'a>,
  },
  Shorthand {
    name: SourceRange<'a>,
  },
  Rest {
    value: Expression<'a>,
  },
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ArrayPatternElement<'a> {
  pub target: Pattern<'a>,
  pub default_value: Option<Expression<'a>>,
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ExportName<'a> {
  // For simplicity, we always set both fields; for shorthands, both nodes are identical.
  pub target: SourceRange<'a>,
  // IdentifierPattern.
  pub alias: Pattern<'a>,
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ExportNames<'a> {
  // `import * as name`
  // `export * from "module"`
  // `export * as name from "module"`
  // IdentifierPattern.
  All(Option<Pattern<'a>>),
  // `import {a as b, c, default as e}`
  // `export {a as default, b as c, d}`
  // `export {default, a as b, c} from "module"`
  // `default` is still a name, so we don't use an enum.
  Specific(SessionVec<'a, ExportName<'a>>),
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct VariableDeclarator<'a> {
  pub pattern: Pattern<'a>,
  pub initializer: Option<Expression<'a>>,
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ForThreeInit<'a> {
  None,
  Expression(Expression<'a>),
  Declaration(Declaration<'a>),
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ForInOfStmtHeaderLhs<'a> {
  Declaration(Declaration<'a>),
  Pattern(Pattern<'a>),
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ForStmtHeader<'a> {
  Three {
    init: ForThreeInit<'a>,
    condition: Option<Expression<'a>>,
    post: Option<Expression<'a>>,
  },
  InOf {
    of: bool,
    lhs: ForInOfStmtHeaderLhs<'a>,
    rhs: Expression<'a>,
  },
}

#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum LiteralTemplatePart<'a> {
  Substitution(Expression<'a>),
  String(SourceRange<'a>),
}

// We no longer derive Eq for the AST due to use of NodeId, as it's not possible to determine structural equality without the node map. Anything that contains a NodeId/Syntax must also avoid Eq.
// WARNING: .clone() is derived and available for shallow copies only. As nodes use NodeId refs to other nodes, it's impossible to actually deep clone from the .clone() method. Use it to quickly copy and make a few changes, while keeping most field values the same (including references to other existing nodes).
#[cfg_attr(feature = "serialize", derive(serde::Serialize, strum_macros::Display))]
#[serde(tag = "$t")]
pub enum Syntax<'a> {
  // Patterns.
  IdentifierPattern {
    name: SourceRange<'a>,
  },
  // `const fn = (a: any, b: any, ...{ length, ...c }: any[]) => void 0` is allowed.
  ArrayPattern {
    // Unnamed elements can exist.
    elements: SessionVec<'a, Option<ArrayPatternElement<'a>>>,
    rest: Option<Pattern<'a>>,
  },
  // For an object pattern, `...` must be followed by an identifier.
  // `const fn = ({ a: { b = c } = d, ...e }: any) => void 0` is possible.
  ObjectPattern {
    // List of ObjectPatternProperty nodes.
    properties: SessionVec<'a, Node<'a>>,
    // This must be IdentifierPattern, anything else is illegal.
    rest: Option<Pattern<'a>>,
  },
  // Not really a pattern but functions similarly; separated out for easy replacement when minifying.
  ClassOrFunctionName {
    name: SourceRange<'a>,
  },

  // Signatures.
  FunctionSignature {
    parameters: SessionVec<'a, Declaration<'a>>,
  },

  // Declarations.
  ClassDecl {
    name: Option<Node<'a>>, // Name can only be omitted in a default export.
    extends: Option<Expression<'a>>,
    members: SessionVec<'a, ClassMember<'a>>,
  },
  FunctionDecl {
    generator: bool,
    is_async: bool,
    name: Option<Node<'a>>, // Name can only be omitted in a default export.
    signature: Node<'a>,
    body: Statement<'a>,
  },
  ParamDecl {
    rest: bool,
    pattern: Pattern<'a>,
    default_value: Option<Expression<'a>>,
  },
  VarDecl {
    mode: VarDeclMode,
    declarators: SessionVec<'a, VariableDeclarator<'a>>,
  },

  // Expressions.
  ArrowFunctionExpr {
    parenthesised: bool,
    is_async: bool,
    signature: Node<'a>,
    body: Node<'a>,
  },
  BinaryExpr {
    parenthesised: bool,
    operator: OperatorName,
    left: Expression<'a>,
    right: Expression<'a>,
  },
  CallExpr {
    optional_chaining: bool,
    parenthesised: bool,
    callee: Expression<'a>,
    arguments: SessionVec<'a, Node<'a>>,
  },
  ClassExpr {
    parenthesised: bool,
    name: Option<Node<'a>>,
    extends: Option<Expression<'a>>,
    members: SessionVec<'a, ClassMember<'a>>,
  },
  ConditionalExpr {
    parenthesised: bool,
    test: Expression<'a>,
    consequent: Expression<'a>,
    alternate: Expression<'a>,
  },
  ComputedMemberExpr {
    optional_chaining: bool,
    object: Expression<'a>,
    member: Expression<'a>,
  },
  FunctionExpr {
    parenthesised: bool,
    is_async: bool,
    generator: bool,
    name: Option<Node<'a>>,
    signature: Node<'a>,
    body: Statement<'a>,
  },
  IdentifierExpr {
    name: SourceRange<'a>,
  },
  ImportExpr {
    module: Expression<'a>,
  },
  ImportMeta {},
  JsxAttribute {
    name: Expression<'a>,          // JsxName
    value: Option<Expression<'a>>, // JsxExpressionContainer or JsxText
  },
  JsxElement {
    name: Option<Expression<'a>>,               // JsxName or JsxMember; None if fragment
    attributes: SessionVec<'a, Expression<'a>>, // JsxAttribute or JsxSpreadAttribute; always empty if fragment
    children: SessionVec<'a, Expression<'a>>,   // JsxElement or JsxExpressionContainer or JsxText
  },
  JsxExpressionContainer {
    value: Expression<'a>,
  },
  JsxMember {
    // This is a separate property to indicate it's required and for easier pattern matching.
    base: SourceRange<'a>,
    path: SessionVec<'a, SourceRange<'a>>,
  },
  JsxName {
    namespace: Option<SourceRange<'a>>,
    name: SourceRange<'a>,
  },
  JsxSpreadAttribute {
    value: Expression<'a>,
  },
  JsxText {
    value: SourceRange<'a>,
  },
  LiteralArrayExpr {
    elements: SessionVec<'a, ArrayElement<'a>>,
  },
  LiteralBigIntExpr {
    value: SessionString<'a>,
  },
  LiteralBooleanExpr {
    value: bool,
  },
  LiteralNull {},
  LiteralNumberExpr {
    value: JsNumber,
  },
  LiteralObjectExpr {
    // List of ObjectMember nodes.
    members: SessionVec<'a, Node<'a>>,
  },
  LiteralRegexExpr {},
  LiteralStringExpr {
    value: SessionString<'a>,
  },
  LiteralTemplateExpr {
    parts: SessionVec<'a, LiteralTemplatePart<'a>>,
  },
  LiteralUndefined {},
  // Dedicated special type to easily distinguish when analysing and minifying. Also done to avoid using IdentifierExpr as right, which is incorrect (not a variable usage).
  MemberExpr {
    parenthesised: bool,
    optional_chaining: bool,
    left: Expression<'a>,
    right: SourceRange<'a>,
  },
  SuperExpr {},
  ThisExpr {},
  UnaryExpr {
    parenthesised: bool,
    operator: OperatorName,
    argument: Expression<'a>,
  },
  UnaryPostfixExpr {
    parenthesised: bool,
    operator: OperatorName,
    argument: Expression<'a>,
  },

  // Statements.
  BlockStmt {
    body: SessionVec<'a, Statement<'a>>,
  },
  BreakStmt {
    label: Option<SourceRange<'a>>,
  },
  ContinueStmt {
    label: Option<SourceRange<'a>>,
  },
  DebuggerStmt {},
  DoWhileStmt {
    condition: Expression<'a>,
    body: Statement<'a>,
  },
  EmptyStmt {},
  ExportDeclStmt {
    declaration: Declaration<'a>,
    default: bool,
  },
  ExportDefaultExprStmt {
    expression: Expression<'a>,
  },
  ExportListStmt {
    names: ExportNames<'a>,
    from: Option<SessionString<'a>>,
  },
  ExpressionStmt {
    expression: Expression<'a>,
  },
  IfStmt {
    test: Expression<'a>,
    consequent: Statement<'a>,
    alternate: Option<Statement<'a>>,
  },
  ImportStmt {
    // IdentifierPattern.
    default: Option<Pattern<'a>>,
    names: Option<ExportNames<'a>>,
    module: SessionString<'a>,
  },
  ForStmt {
    header: ForStmtHeader<'a>,
    body: Statement<'a>,
  },
  LabelStmt {
    name: SourceRange<'a>,
    statement: Statement<'a>,
  },
  ReturnStmt {
    value: Option<Expression<'a>>,
  },
  SwitchStmt {
    test: Expression<'a>,
    branches: SessionVec<'a, Node<'a>>,
  },
  ThrowStmt {
    value: Expression<'a>,
  },
  TryStmt {
    wrapped: Statement<'a>,
    // One of these must be present.
    catch: Option<Node<'a>>,
    finally: Option<Statement<'a>>,
  },
  VarStmt {
    declaration: Declaration<'a>,
  },
  WhileStmt {
    condition: Expression<'a>,
    body: Statement<'a>,
  },

  // Others.
  TopLevel {
    body: SessionVec<'a, Statement<'a>>,
  },
  CallArg {
    spread: bool,
    value: Expression<'a>,
  },
  CatchBlock {
    parameter: Option<Pattern<'a>>,
    body: Statement<'a>,
  },
  // This is a node instead of an enum so that we can replace it when minifying e.g. expanding shorthand to `key: value`.
  ObjectMember {
    typ: ObjectMemberType<'a>,
  },
  ObjectPatternProperty {
    key: ClassOrObjectMemberKey<'a>,
    // Omitted if shorthand i.e. key is Direct and target is IdentifierPattern of same name.
    // TODO Ideally for simplicity this should be duplicated from `key` if shorthand, with a `shorthand` boolean field to indicate so.
    target: Option<Pattern<'a>>,
    default_value: Option<Expression<'a>>,
  },
  SwitchBranch {
    // If None, it's `default`.
    case: Option<Expression<'a>>,
    body: SessionVec<'a, Statement<'a>>,
  },
}
