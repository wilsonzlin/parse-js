use crate::error::SyntaxError;
use crate::error::SyntaxErrorType;
use crate::loc::Loc;
use ahash::HashSet;
use ahash::HashSetExt;
use once_cell::sync::Lazy;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum TokenType {
  // Used to represent a type that should never be seen in actual code. Similar to 0xFF from UTF-8
  // bytes perspective. Often used to represent an omitted value without having to use `Option`.
  _Dummy,
  // Special token used to represent the end of the source code. Easier than using and handling Option everywhere.
  EOF,

  Ampersand,
  AmpersandAmpersand,
  AmpersandAmpersandEquals,
  AmpersandEquals,
  Asterisk,
  AsteriskAsterisk,
  AsteriskAsteriskEquals,
  AsteriskEquals,
  Bar,
  BarBar,
  BarBarEquals,
  BarEquals,
  BraceClose,
  BraceOpen,
  BracketClose,
  BracketOpen,
  Caret,
  CaretEquals,
  ChevronLeft,
  ChevronLeftChevronLeft,
  ChevronLeftChevronLeftEquals,
  ChevronLeftEquals,
  ChevronLeftSlash,
  ChevronRight,
  ChevronRightChevronRight,
  ChevronRightChevronRightChevronRight,
  ChevronRightChevronRightChevronRightEquals,
  ChevronRightChevronRightEquals,
  ChevronRightEquals,
  Colon,
  Comma,
  CommentMultiple,
  CommentSingle,
  Dot,
  DotDotDot,
  Equals,
  EqualsChevronRight,
  EqualsEquals,
  EqualsEqualsEquals,
  Exclamation,
  ExclamationEquals,
  ExclamationEqualsEquals,
  Hyphen,
  HyphenEquals,
  HyphenHyphen,
  Identifier,
  JsxTextContent,
  KeywordAs,
  KeywordAsync,
  KeywordAwait,
  KeywordBreak,
  KeywordCase,
  KeywordCatch,
  KeywordClass,
  KeywordConst,
  KeywordConstructor,
  KeywordContinue,
  KeywordDebugger,
  KeywordDefault,
  KeywordDelete,
  KeywordDo,
  KeywordElse,
  KeywordEnum,
  KeywordExport,
  KeywordExtends,
  KeywordFinally,
  KeywordFor,
  KeywordFrom,
  KeywordFunction,
  KeywordGet,
  KeywordIf,
  KeywordImport,
  KeywordIn,
  KeywordInstanceof,
  KeywordLet,
  KeywordNew,
  KeywordOf,
  KeywordReturn,
  KeywordSet,
  KeywordStatic,
  KeywordSuper,
  KeywordSwitch,
  KeywordThis,
  KeywordThrow,
  KeywordTry,
  KeywordTypeof,
  KeywordVar,
  KeywordVoid,
  KeywordWhile,
  KeywordWith,
  KeywordYield,
  LiteralBigInt,
  LiteralFalse,
  LiteralNull,
  LiteralNumber,
  // LiteralNumber* are only used for lexing
  LiteralNumberHex,
  LiteralNumberBin,
  LiteralNumberOct,
  LiteralRegex,
  LiteralString,
  LiteralTemplatePartString,
  LiteralTemplatePartStringEnd,
  LiteralTrue,
  ParenthesisClose,
  ParenthesisOpen,
  Percent,
  PercentEquals,
  Plus,
  PlusEquals,
  PlusPlus,
  PrivateMember,
  Question,
  QuestionDot,
  QuestionDotBracketOpen,
  QuestionDotParenthesisOpen,
  QuestionQuestion,
  QuestionQuestionEquals,
  Semicolon,
  Slash,
  SlashEquals,
  Tilde,
}

// These can be used as parameter and variable names.
pub static UNRESERVED_KEYWORDS: Lazy<HashSet<TokenType>> = Lazy::new(|| {
  let mut set = HashSet::<TokenType>::new();
  set.insert(TokenType::KeywordAs);
  set.insert(TokenType::KeywordAsync);
  set.insert(TokenType::KeywordConstructor);
  set.insert(TokenType::KeywordFrom);
  set.insert(TokenType::KeywordGet);
  set.insert(TokenType::KeywordLet);
  set.insert(TokenType::KeywordOf);
  set.insert(TokenType::KeywordSet);
  set.insert(TokenType::KeywordStatic);
  set
});

#[derive(Clone, Debug)]
pub struct Token {
  pub loc: Loc,
  // Whether one or more whitespace characters appear immediately before this token, and at least
  // one of those whitespace characters is a line terminator.
  pub preceded_by_line_terminator: bool,
  pub typ: TokenType,
}

impl Token {
  pub fn new(loc: Loc, typ: TokenType, preceded_by_line_terminator: bool) -> Token {
    Token {
      loc,
      typ,
      preceded_by_line_terminator,
    }
  }

  pub fn error(&self, typ: SyntaxErrorType) -> SyntaxError {
    self.loc.error(typ, Some(self.typ))
  }
}
