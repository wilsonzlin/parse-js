use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::loc::Loc;
use crate::num::JsNumber;
use crate::operator::OperatorName;
use ahash::AHashMap;
use core::fmt::Debug;
use serde::Serialize;
use serde::Serializer;
use std::any::Any;
use std::any::TypeId;
use std::fmt;
use std::fmt::Formatter;

#[derive(Default)]
pub struct NodeAssocData {
  map: AHashMap<TypeId, Box<dyn Any>>,
}

impl NodeAssocData {
  pub fn get<T: Any>(&self) -> Option<&T> {
    let t = TypeId::of::<T>();
    self.map.get(&t).map(|v| v.downcast_ref().unwrap())
  }

  pub fn set<T: Any>(&mut self, v: T) {
    let t = TypeId::of::<T>();
    self.map.insert(t, Box::from(v));
  }
}

#[cfg(test)]
mod tests {
  use crate::ast::NodeAssocData;

  #[test]
  fn test_node_assoc_data() {
    struct MyType(u32);
    let mut assoc = NodeAssocData::default();
    assoc.set(MyType(32));
    let v = assoc.get::<MyType>().unwrap();
    assert_eq!(v.0, 32);
  }
}

pub struct Node {
  // A location is not a SourceRange; consider that after some transformations, it's possible to create entirely new nodes that don't exist at all in the source code. Also, sometimes we cannot be bothered to set a location, or can only provide an approximate/best-effort location.
  pub loc: Loc,
  pub stx: Box<Syntax>,
  pub assoc: NodeAssocData,
}

impl Node {
  pub fn new(loc: Loc, stx: Syntax) -> Node {
    Node {
      loc,
      stx: Box::new(stx),
      assoc: NodeAssocData::default(),
    }
  }

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

impl Serialize for Node {
  fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
    self.stx.serialize(serializer)
  }
}

// These are for readability only, and do not increase type safety or define different structures.
type Declaration = Node;
type Expression = Node;
type Pattern = Node;
type Statement = Node;

#[derive(Eq, PartialEq, Clone, Copy, Debug, Serialize)]
pub enum VarDeclMode {
  Const,
  Let,
  Var,
}

#[derive(Debug, Serialize)]
pub enum ArrayElement {
  Single(Expression),
  Rest(Expression),
  Empty,
}

// WARNING: This enum must exist, and the two variants cannot be merged by representing Direct with an IdentifierExpr, as it's not a usage of a variable!
#[derive(Debug, Serialize)]
pub enum ClassOrObjectMemberKey {
  // Identifier, keyword, string, or number.
  // NOTE: This isn't used by ObjectMemberType::Shorthand.
  Direct(String),
  Computed(Expression),
}

#[derive(Debug, Serialize)]
pub enum ClassOrObjectMemberValue {
  Getter {
    function: Node, // Always Function. `params` is empty.
  },
  Method {
    function: Node, // Always Function.
  },
  Property {
    // Must be Some if object, as shorthands are covered by ObjectMemberType::Shorthand (and are initialised).
    initializer: Option<Expression>,
  },
  Setter {
    function: Node, // Always Function. `params` contains exactly one ParamDecl with no `default_value` or `rest`.
  },
}

#[derive(Debug, Serialize)]
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

#[derive(Debug, Serialize)]
pub struct ArrayPatternElement {
  pub target: Pattern,
  pub default_value: Option<Expression>,
}

#[derive(Debug, Serialize)]
pub struct ExportName {
  // For simplicity, we always set both fields; for shorthands, both nodes are identical.
  pub target: String,
  // IdentifierPattern.
  pub alias: Pattern,
}

#[derive(Debug, Serialize)]
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

#[derive(Debug, Serialize)]
pub struct VariableDeclarator {
  pub pattern: Pattern,
  pub initializer: Option<Expression>,
}

#[derive(Debug, Serialize)]
pub enum ForInit {
  None,
  Expression(Expression),
  Declaration(Declaration),
}

#[derive(Debug, Serialize)]
pub enum LiteralTemplatePart {
  Substitution(Expression),
  String(String),
}

#[derive(Debug, Serialize)]
#[serde(tag = "$t")]
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

  // Functions.
  // This common type exists for better downstream usage, as one type is easier to match on and wrangle than many different types (ArrowFunctionExpr, ClassMember::Method, FunctionDecl, etc.).
  Function {
    async_: bool,
    generator: bool,
    parameters: Vec<Declaration>, // Always ParamDecl.
    body: Node, // Could be Expression if arrow function. Otherwise, it's FunctionBody.
  },
  // A function body is different from a block statement, as the scopes are different. This doesn't mean much at the parser level, but helps with downstream usages.
  FunctionBody {
    body: Vec<Statement>,
  },

  // Declarations.
  ClassDecl {
    export: bool,
    export_default: bool,
    name: Option<Node>, // Always ClassOrFunctionName. Name can only be omitted in a default export, although a default export class can still have a name.
    extends: Option<Expression>,
    members: Vec<Node>, // Always ClassMember.
  },
  FunctionDecl {
    export: bool,
    export_default: bool,
    name: Option<Node>, // Always ClassOrFunctionName. Name can only be omitted in a default export, although a default export function can still have a name.
    function: Node,     // Always Function.
  },
  ParamDecl {
    rest: bool,
    pattern: Pattern,
    default_value: Option<Expression>,
  },
  VarDecl {
    export: bool,
    mode: VarDeclMode,
    declarators: Vec<VariableDeclarator>,
  },

  // Expressions.
  ArrowFunctionExpr {
    parenthesised: bool,
    function: Node, // Always Function.
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
    members: Vec<Node>, // Always ClassMember.
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
    name: Option<Node>,
    function: Node,
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
  LiteralRegexExpr {
    value: String, // Including delimiter slashes and any flags.
  },
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
    body: Statement, // Won't be BlockStmt, but ForBody instead. (However, could be another type of statement.)
  },
  ForInStmt {
    // for-in and for-of statements can have `x`/`[x]`/`{x:a}`/etc. on the lhs or `var x`/`var [x]`/etc. on the lhs. But for the latter, while it's technically a Decl, it's always a VarDecl with exactly one declaration that has no initialiser. If you strip down VarDecl to this, it's basically just a VarDeclMode and a Pattern. Therefore, we can represent both a destructuring expr or a decl on the lhs with an Option<VarDeclMode> and a Pattern.
    decl_mode: Option<VarDeclMode>,
    pat: Pattern,
    rhs: Expression,
    body: Statement, // Won't be BlockStmt, but ForBody instead. (However, could be another type of statement.)
  },
  ForOfStmt {
    await_: bool,
    // See comment in ForInStmt.
    decl_mode: Option<VarDeclMode>,
    pat: Pattern,
    rhs: Expression,
    body: Statement, // Won't be BlockStmt, but ForBody instead. (However, could be another type of statement.)
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
    body: Vec<Statement>, // We don't want to use BlockStmt as the new block scope starts with the parameter, not the braces. This differentiation ensures BlockStmt specifically means a new scope, helpful for downstream usages. See also: FunctionBody.
  },
  ClassMember {
    key: ClassOrObjectMemberKey,
    static_: bool,
    value: ClassOrObjectMemberValue,
  },
  // Similar purpose to CatchBlock and FunctionBody. (The scope for a `for` statement starts before the braces, so don't mix with BlockStmt.)
  ForBody {
    body: Vec<Statement>,
  },
  // This is a node instead of an enum so that we can replace it when minifying e.g. expanding shorthand to `key: value`.
  ObjectMember {
    typ: ObjectMemberType,
  },
  ObjectPatternProperty {
    key: ClassOrObjectMemberKey,
    // If `shorthand`, `key` is Direct and `target` is IdentifierPattern of same name. This way, there is always an IdentifierPattern that exists and can be visited, instead of also having to consider ClassOrObjectMemberKey::Direct as identifier if shorthand.
    target: Pattern,
    shorthand: bool,
    default_value: Option<Expression>,
  },
  SwitchBranch {
    // If None, it's `default`.
    case: Option<Expression>,
    body: Vec<Statement>,
  },
}
