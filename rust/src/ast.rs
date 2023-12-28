use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::flag::flag_bitfield;
use crate::flag::Flag;
use crate::flag::Flags;
use crate::num::JsNumber;
use crate::operator::OperatorName;
use crate::session::Session;
use crate::session::SessionString;
use crate::session::SessionVec;
use crate::source::SourceRange;
use crate::symbol::Scope;
use core::fmt::Debug;
use std::fmt;
use std::fmt::Formatter;
use std::ops::BitOr;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeFlag {
  HasClassDecl,                            // BlockStmt
  HasConstVarDeclWithIdentifierPattern,    // BlockStmt
  HasConstVarDeclWithNonIdentifierPattern, // BlockStmt
  HasFunctionDecl,                         // BlockStmt
  HasLetVarDeclWithIdentifierPattern,      // BlockStmt
  HasLetVarDeclWithNonIdentifierPattern,   // BlockStmt
  HasNonExprStmtOrVarDecl,                 // BlockStmt
  HasVarVarDeclWithIdentifierPattern,      // BlockStmt
  HasVarVarDeclWithNonIdentifierPattern,   // BlockStmt
  UnconditionallyBreaks,                   // BlockStmt, BreakStmt, DoWhileStmt IfStmt, TryStmt
  UnconditionallyContinues,                // BlockStmt, ContinueStmt, DoWhileStmt, IfStmt, TryStmt
  UnconditionallyReturns,                  // BlockStmt, DoWhileStmt, IfStmt, ReturnStmt, TryStmt
  UnconditionallyThrows,                   // BlockStmt, DoWhileStmt, IfStmt, ThrowStmt, TryStmt
  Unreachable,                             // Any child of BlockStmt
}

impl Flag for NodeFlag {
  fn bitfield(self) -> u64 {
    flag_bitfield!(self)
  }
}

impl BitOr for NodeFlag {
  type Output = Flags<NodeFlag>;

  fn bitor(self, rhs: Self) -> Self::Output {
    Flags::from_raw(self.bitfield() | rhs.bitfield())
  }
}

pub const NODE_FLAG_UNCONDITIONAL_FLOWS: Flags<NodeFlag> = Flags::from_raw(
  flag_bitfield!(NodeFlag::UnconditionallyBreaks)
    | flag_bitfield!(NodeFlag::UnconditionallyContinues)
    | flag_bitfield!(NodeFlag::UnconditionallyReturns)
    | flag_bitfield!(NodeFlag::UnconditionallyThrows),
);

// To prevent ambiguity and confusion, don't derive Eq, as there are multiple meanings of "equality" for nodes:
// - Exact identical instances, so two different nodes with the same syntax, location, and scope would still be different.
// - Same syntax, location, and scope.
// - Same syntax and location, but scope can be different.
// - Same syntax, but location and scope can be different.
pub struct NodeData<'a> {
  pub loc: SourceRange<'a>,
  pub stx: Syntax<'a>,
  // For the purposes of disambiguation, the scope of a function or block is only set on its children and not itself. This is merely an arbitrary decision. For example, the scope created by a function is assigned to its signature nodes (and descendants e.g. default values), but not to the FunctionStmt itself. For a `for` loop, the scope created by it is assigned to its header nodes and descendants, but not to the ForStmt itself. For a block statement, the scope created by it is assigned to statements inside it, but not to the BlockStmt itself.
  pub scope: Scope<'a>,
  pub flags: Flags<NodeFlag>,
}

impl<'a> NodeData<'a> {
  /// Move this node into a new node (also allocated on the arena and returned as a mutable reference). This node will be left in an invalid state and must not be used any further.
  pub fn take(&mut self, session: &'a Session) -> &'a mut NodeData<'a> {
    self.replace(session, Syntax::_TakenNode {})
  }

  pub fn replace(&mut self, session: &'a Session, stx: Syntax<'a>) -> &'a mut NodeData<'a> {
    let dummy = NodeData {
      loc: self.loc,
      scope: self.scope,
      stx,
      flags: self.flags,
    };
    let taken = core::mem::replace(self, dummy);
    session.get_allocator().alloc(taken)
  }

  /// Create an error at this node's location.
  pub fn error(&self, typ: SyntaxErrorType) -> SyntaxError<'a> {
    self.loc.error(typ, None)
  }
}

impl<'a> Debug for Node<'a> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.stx.fmt(f)
  }
}

#[cfg(feature = "serialize")]
impl<'a> serde::Serialize for NodeData<'a> {
  fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.stx.serialize(serializer)
  }
}

pub type Node<'a> = &'a mut NodeData<'a>;

pub fn new_node_with_flags<'a>(
  session: &'a Session,
  scope: Scope<'a>,
  loc: SourceRange<'a>,
  stx: Syntax<'a>,
  flags: Flags<NodeFlag>,
) -> Node<'a> {
  session.mem.alloc(NodeData {
    loc,
    stx,
    scope,
    flags,
  })
}

pub fn new_node_with_flag<'a>(
  session: &'a Session,
  scope: Scope<'a>,
  loc: SourceRange<'a>,
  stx: Syntax<'a>,
  flag: NodeFlag,
) -> Node<'a> {
  new_node_with_flags(session, scope, loc, stx, Flags::new() | flag)
}

pub fn new_node<'a>(
  session: &'a Session,
  scope: Scope<'a>,
  loc: SourceRange<'a>,
  stx: Syntax<'a>,
) -> Node<'a> {
  new_node_with_flags(session, scope, loc, stx, Flags::new())
}

// These are for readability only, and do not increase type safety or define different structures.
type Declaration<'a> = Node<'a>;
type Expression<'a> = Node<'a>;
type Pattern<'a> = Node<'a>;
type Statement<'a> = Node<'a>;

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum VarDeclMode {
  Const,
  Let,
  Var,
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ArrayElement<'a> {
  Single(Expression<'a>),
  Rest(Expression<'a>),
  Empty,
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ClassOrObjectMemberKey<'a> {
  // Identifier, keyword, string, or number.
  // NOTE: This isn't used by ObjectMemberType::Shorthand.
  Direct(SourceRange<'a>),
  Computed(Expression<'a>),
}

impl<'a> ClassOrObjectMemberKey<'a> {
  pub fn take(&mut self) -> ClassOrObjectMemberKey<'a> {
    core::mem::replace(
      self,
      ClassOrObjectMemberKey::Direct(SourceRange::from_slice(b"")),
    )
  }
}

#[derive(Debug)]
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

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ClassMember<'a> {
  pub key: ClassOrObjectMemberKey<'a>,
  pub statik: bool,
  pub value: ClassOrObjectMemberValue<'a>,
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ObjectMemberType<'a> {
  Valued {
    key: ClassOrObjectMemberKey<'a>,
    value: ClassOrObjectMemberValue<'a>,
  },
  Shorthand {
    identifier: Node<'a>, // Always IdentifierExpr.
  },
  Rest {
    value: Expression<'a>,
  },
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ArrayPatternElement<'a> {
  pub target: Pattern<'a>,
  pub default_value: Option<Expression<'a>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ExportName<'a> {
  // For simplicity, we always set both fields; for shorthands, both nodes are identical.
  pub target: SourceRange<'a>,
  // IdentifierPattern.
  pub alias: Pattern<'a>,
}

#[derive(Debug)]
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

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct VariableDeclarator<'a> {
  pub pattern: Pattern<'a>,
  pub initializer: Option<Expression<'a>>,
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum ForInit<'a> {
  None,
  Expression(Expression<'a>),
  Declaration(Declaration<'a>),
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum LiteralTemplatePart<'a> {
  Substitution(Expression<'a>),
  String(&'a str),
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize, strum_macros::Display))]
#[cfg_attr(feature = "serialize", serde(tag = "$t"))]
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
    name: Option<Node<'a>>, // Name can only be omitted in a default export, although a default export class can still have a name.
    extends: Option<Expression<'a>>,
    members: SessionVec<'a, ClassMember<'a>>,
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
    name: Option<Node<'a>>, // Name can only be omitted in a default export, although a default export function can still have a name.
    signature: Node<'a>,
    body: Statement<'a>,
  },
  ParamDecl {
    rest: bool,
    pattern: Pattern<'a>,
    default_value: Option<Expression<'a>>,
  },
  VarDecl {
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    export: bool,
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
    /// Set to `true` if this MemberExpr is the LHS of an assignment expression.
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    assignment_target: bool,
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
    // When an element name starts with a lowercase ASCII character, it's a built-in component like '<div>' or '<span>'.
    // For easier differentiation, we use IdentifierExpr for user-defined components as they are references to symbols and built-in components are not.
    // https://reactjs.org/docs/jsx-in-depth.html#user-defined-components-must-be-capitalized
    name: Option<Expression<'a>>, // IdentifierExpr or JsxName or JsxMemberExpression; None if fragment
    attributes: SessionVec<'a, Expression<'a>>, // JsxAttribute or JsxSpreadAttribute; always empty if fragment
    children: SessionVec<'a, Expression<'a>>,   // JsxElement or JsxExpressionContainer or JsxText
  },
  JsxExpressionContainer {
    value: Expression<'a>,
  },
  JsxMemberExpression {
    // This is a separate property to indicate it's required and for easier pattern matching.
    base: Node<'a>, // Always IdentifierExpr
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
    value: &'a str,
  },
  LiteralTemplateExpr {
    parts: SessionVec<'a, LiteralTemplatePart<'a>>,
  },
  // Dedicated special type to easily distinguish when analysing and minifying. Also done to avoid using IdentifierExpr as right, which is incorrect (not a variable usage).
  MemberExpr {
    parenthesised: bool,
    optional_chaining: bool,
    /// Set to `true` if this MemberExpr is the LHS of an assignment expression.
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    assignment_target: bool,
    left: Expression<'a>,
    right: SourceRange<'a>,
  },
  SuperExpr {},
  ThisExpr {},
  TaggedTemplateExpr {
    function: Expression<'a>,
    parts: SessionVec<'a, LiteralTemplatePart<'a>>,
  },
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
  ExportDefaultExprStmt {
    expression: Expression<'a>,
  },
  ExportListStmt {
    names: ExportNames<'a>,
    from: Option<&'a str>,
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
    module: &'a str,
  },
  ForStmt {
    init: ForInit<'a>,
    condition: Option<Expression<'a>>,
    post: Option<Expression<'a>>,
    body: Statement<'a>,
  },
  ForInStmt {
    // for-in and for-of statements can have `x`/`[x]`/`{x:a}`/etc. on the lhs or `var x`/`var [x]`/etc. on the lhs. But for the latter, while it's technically a Decl, it's always a VarDecl with exactly one declaration that has no initialiser. If you strip down VarDecl to this, it's basically just a VarDeclMode and a Pattern. Therefore, we can represent both a destructuring expr or a decl on the lhs with an Option<VarDeclMode> and a Pattern.
    decl_mode: Option<VarDeclMode>,
    pat: Pattern<'a>,
    rhs: Expression<'a>,
    body: Statement<'a>,
  },
  ForOfStmt {
    #[cfg_attr(feature = "serialize", serde(default))]
    await_: bool,
    // See comment in ForInStmt.
    decl_mode: Option<VarDeclMode>,
    pat: Pattern<'a>,
    rhs: Expression<'a>,
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
    // If `shorthand`, `key` is Direct and `target` is IdentifierPattern of same name. This way, there is always an IdentifierPattern that exists and can be visited, instead of also having to consider ClassOrObjectMemberKey::Direct as identifier if shorthand.
    target: Pattern<'a>,
    #[cfg_attr(feature = "serialize", serde(default))]
    #[cfg_attr(
      feature = "serialize",
      serde(skip_serializing_if = "core::ops::Not::not")
    )]
    shorthand: bool,
    default_value: Option<Expression<'a>>,
  },
  SwitchBranch {
    // If None, it's `default`.
    case: Option<Expression<'a>>,
    body: SessionVec<'a, Statement<'a>>,
  },
  // This should never be seen. Always assert that it's unreachable.
  _TakenNode {},
}
