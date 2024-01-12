use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::loc::Loc;
use crate::num::JsNumber;
use crate::operator::OperatorName;
use core::fmt::Debug;
use std::fmt;
use std::fmt::Formatter;

// To prevent ambiguity and confusion, don't derive Eq, as there are multiple meanings of "equality" for nodes:
// - Exact identical instances, so two different nodes with the same syntax, location, and scope would still be different.
// - Same syntax, location, and scope.
// - Same syntax and location, but scope can be different.
// - Same syntax, but location and scope can be different.
#[derive(Clone)]
pub struct Node {
  // A location is not a SourceRange; consider that after some transformations, it's possible to create entirely new nodes that don't exist at all in the source code. Also, sometimes we cannot be bothered to set a location, or can only provide an approximate/best-effort location.
  pub loc: Loc,
  pub stx: Box<Syntax>,
}

impl Node {
  /// Create an error at this node's location.
  pub fn error(&self, typ: SyntaxErrorType) -> SyntaxError {
    self.loc.error(typ, None)
  }

  pub fn as_ident(&self) -> &str {
    match self.stx.as_ref() {
      Syntax::IdentifierExpr { name } => name.as_str(),
      _ => unreachable!(),
    }
  }
}

impl Debug for Node {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.stx.fmt(f)
  }
}

#[cfg(feature = "serialize")]
impl serde::Serialize for Node {
  fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.stx.serialize(serializer)
  }
}

// These are for readability only, and do not increase type safety or define different structures.
type Declaration = Node;
type Expression = Node;
type Pattern = Node;
type Statement = Node;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum VarDeclMode {
  Const,
  Let,
  Var,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ArrayElement {
  Single(Expression),
  Rest(Expression),
  Empty,
}

// WARNING: This enum must exist, and the two variants cannot be merged by representing Direct with an IdentifierExpr, as it's not a usage of a variable!
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ClassOrObjectMemberKey {
  // Identifier, keyword, string, or number.
  // NOTE: This isn't used by ObjectMemberType::Shorthand.
  Direct(String),
  Computed(Expression),
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ClassOrObjectMemberValue {
  Getter {
    body: Statement,
  },
  Method {
    is_async: bool,
    generator: bool,
    signature: Node,
    body: Statement,
  },
  Property {
    // Must be Some if object, as shorthands are covered by ObjectMemberType::Shorthand (and are initialised).
    initializer: Option<Expression>,
  },
  Setter {
    body: Statement,
    parameter: Pattern,
  },
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ClassMember {
  pub key: ClassOrObjectMemberKey,
  pub static_: bool,
  pub value: ClassOrObjectMemberValue,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ObjectMemberType {
  Valued {
    key: ClassOrObjectMemberKey,
    value: ClassOrObjectMemberValue,
  },
  Shorthand {
    identifier: Node, // Always IdentifierExpr.
  },
  Rest {
    value: Expression,
  },
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ArrayPatternElement {
  pub target: Pattern,
  pub default_value: Option<Expression>,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ExportName {
  // For simplicity, we always set both fields; for shorthands, both nodes are identical.
  pub target: String,
  // IdentifierPattern.
  pub alias: Pattern,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ExportNames {
  // `import * as name`
  // `export * from "module"`
  // `export * as name from "module"`
  // IdentifierPattern.
  All(Option<Pattern>),
  // `import {a as b, c, default as e}`
  // `export {a as default, b as c, d}`
  // `export {default, a as b, c} from "module"`
  // `default` is still a name, so we don't use an enum.
  Specific(Vec<ExportName>),
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct VariableDeclarator {
  pub pattern: Pattern,
  pub initializer: Option<Expression>,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ForInit {
  None,
  Expression(Expression),
  Declaration(Declaration),
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum LiteralTemplatePart {
  Substitution(Expression),
  String(String),
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, strum_macros::Display))]
#[cfg_attr(feature = "serialize", serde(tag = "$t"))]
pub enum Syntax {
  // Patterns.
  IdentifierPattern {
    name: String,
  },
  // `const fn = (a: any, b: any, ...{ length, ...c }: any[]) => void 0` is allowed.
  ArrayPattern {
    // Unnamed elements can exist.
    elements: Vec<Option<ArrayPatternElement>>,
    rest: Option<Pattern>,
  },
  // For an object pattern, `...` must be followed by an identifier.
  // `const fn = ({ a: { b = c } = d, ...e }: any) => void 0` is possible.
  ObjectPattern {
    // List of ObjectPatternProperty nodes.
    properties: Vec<Node>,
    // This must be IdentifierPattern, anything else is illegal.
    rest: Option<Pattern>,
  },
  // Not really a pattern but functions similarly; separated out for easy replacement when minifying.
  ClassOrFunctionName {
    name: String,
  },

  // Signatures.
  FunctionSignature {
    parameters: Vec<Declaration>,
  },

  // Declarations.
  ClassDecl {
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    export: bool,
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    export_default: bool,
    name: Option<Node>, // Name can only be omitted in a default export, although a default export class can still have a name.
    extends: Option<Expression>,
    members: Vec<ClassMember>,
  },
  FunctionDecl {
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    export: bool,
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    export_default: bool,
    generator: bool,
    is_async: bool,
    name: Option<Node>, // Name can only be omitted in a default export, although a default export function can still have a name.
    signature: Node,
    body: Statement,
  },
  ParamDecl {
    rest: bool,
    pattern: Pattern,
    default_value: Option<Expression>,
  },
  VarDecl {
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    export: bool,
    mode: VarDeclMode,
    declarators: Vec<VariableDeclarator>,
  },

  // Expressions.
  ArrowFunctionExpr {
    parenthesised: bool,
    is_async: bool,
    signature: Node,
    body: Node,
  },
  BinaryExpr {
    parenthesised: bool,
    operator: OperatorName,
    left: Expression,
    right: Expression,
  },
  CallExpr {
    optional_chaining: bool,
    parenthesised: bool,
    callee: Expression,
    arguments: Vec<Node>,
  },
  ClassExpr {
    parenthesised: bool,
    name: Option<Node>,
    extends: Option<Expression>,
    members: Vec<ClassMember>,
  },
  ConditionalExpr {
    parenthesised: bool,
    test: Expression,
    consequent: Expression,
    alternate: Expression,
  },
  ComputedMemberExpr {
    optional_chaining: bool,
    object: Expression,
    member: Expression,
  },
  FunctionExpr {
    parenthesised: bool,
    is_async: bool,
    generator: bool,
    name: Option<Node>,
    signature: Node,
    body: Statement,
  },
  IdentifierExpr {
    name: String,
  },
  ImportExpr {
    module: Expression,
  },
  ImportMeta {},
  JsxAttribute {
    name: Expression,          // JsxName
    value: Option<Expression>, // JsxExpressionContainer or JsxText
  },
  JsxElement {
    // When an element name starts with a lowercase ASCII character, it's a built-in component like '<div>' or '<span>'.
    // For easier differentiation, we use IdentifierExpr for user-defined components as they are references to symbols and built-in components are not.
    // https://reactjs.org/docs/jsx-in-depth.html#user-defined-components-must-be-capitalized
    name: Option<Expression>, // IdentifierExpr or JsxName or JsxMemberExpression; None if fragment
    attributes: Vec<Expression>, // JsxAttribute or JsxSpreadAttribute; always empty if fragment
    children: Vec<Expression>, // JsxElement or JsxExpressionContainer or JsxText
  },
  JsxExpressionContainer {
    value: Expression,
  },
  JsxMemberExpression {
    // This is a separate property to indicate it's required and for easier pattern matching.
    base: Node, // Always IdentifierExpr
    path: Vec<String>,
  },
  JsxName {
    namespace: Option<String>,
    name: String,
  },
  JsxSpreadAttribute {
    value: Expression,
  },
  JsxText {
    value: String,
  },
  LiteralArrayExpr {
    elements: Vec<ArrayElement>,
  },
  LiteralBigIntExpr {
    value: String,
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
    members: Vec<Node>,
  },
  LiteralRegexExpr {},
  LiteralStringExpr {
    value: String,
  },
  LiteralTemplateExpr {
    parts: Vec<LiteralTemplatePart>,
  },
  // Dedicated special type to easily distinguish when analysing and minifying. Also done to avoid using IdentifierExpr as right, which is incorrect (not a variable usage).
  MemberExpr {
    parenthesised: bool,
    optional_chaining: bool,
    left: Expression,
    right: String,
  },
  SuperExpr {},
  ThisExpr {},
  TaggedTemplateExpr {
    function: Expression,
    parts: Vec<LiteralTemplatePart>,
  },
  UnaryExpr {
    parenthesised: bool,
    operator: OperatorName,
    argument: Expression,
  },
  UnaryPostfixExpr {
    parenthesised: bool,
    operator: OperatorName,
    argument: Expression,
  },

  // Statements.
  BlockStmt {
    body: Vec<Statement>,
  },
  BreakStmt {
    label: Option<String>,
  },
  ContinueStmt {
    label: Option<String>,
  },
  DebuggerStmt {},
  DoWhileStmt {
    condition: Expression,
    body: Statement,
  },
  EmptyStmt {},
  ExportDefaultExprStmt {
    expression: Expression,
  },
  ExportListStmt {
    names: ExportNames,
    from: Option<String>,
  },
  ExpressionStmt {
    expression: Expression,
  },
  IfStmt {
    test: Expression,
    consequent: Statement,
    alternate: Option<Statement>,
  },
  ImportStmt {
    // IdentifierPattern.
    default: Option<Pattern>,
    names: Option<ExportNames>,
    module: String,
  },
  ForStmt {
    init: ForInit,
    condition: Option<Expression>,
    post: Option<Expression>,
    body: Statement,
  },
  ForInStmt {
    // for-in and for-of statements can have `x`/`[x]`/`{x:a}`/etc. on the lhs or `var x`/`var [x]`/etc. on the lhs. But for the latter, while it's technically a Decl, it's always a VarDecl with exactly one declaration that has no initialiser. If you strip down VarDecl to this, it's basically just a VarDeclMode and a Pattern. Therefore, we can represent both a destructuring expr or a decl on the lhs with an Option<VarDeclMode> and a Pattern.
    decl_mode: Option<VarDeclMode>,
    pat: Pattern,
    rhs: Expression,
    body: Statement,
  },
  ForOfStmt {
    #[cfg_attr(feature = "serialize", serde(default))]
    await_: bool,
    // See comment in ForInStmt.
    decl_mode: Option<VarDeclMode>,
    pat: Pattern,
    rhs: Expression,
    body: Statement,
  },
  LabelStmt {
    name: String,
    statement: Statement,
  },
  ReturnStmt {
    value: Option<Expression>,
  },
  SwitchStmt {
    test: Expression,
    branches: Vec<Node>,
  },
  ThrowStmt {
    value: Expression,
  },
  TryStmt {
    wrapped: Statement,
    // One of these must be present.
    catch: Option<Node>,
    finally: Option<Statement>,
  },
  WhileStmt {
    condition: Expression,
    body: Statement,
  },

  // Others.
  TopLevel {
    body: Vec<Statement>,
  },
  CallArg {
    spread: bool,
    value: Expression,
  },
  CatchBlock {
    parameter: Option<Pattern>,
    body: Statement,
  },
  // This is a node instead of an enum so that we can replace it when minifying e.g. expanding shorthand to `key: value`.
  ObjectMember {
    typ: ObjectMemberType,
  },
  ObjectPatternProperty {
    key: ClassOrObjectMemberKey,
    // If `shorthand`, `key` is Direct and `target` is IdentifierPattern of same name. This way, there is always an IdentifierPattern that exists and can be visited, instead of also having to consider ClassOrObjectMemberKey::Direct as identifier if shorthand.
    target: Pattern,
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    shorthand: bool,
    default_value: Option<Expression>,
  },
  SwitchBranch {
    // If None, it's `default`.
    case: Option<Expression>,
    body: Vec<Statement>,
  },
}
