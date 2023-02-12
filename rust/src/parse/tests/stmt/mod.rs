use crate::lex::Lexer;
use crate::parse::pattern::ParsePatternRules;
use crate::parse::ParseCtx;
use crate::parse::Parser;
use crate::session::Session;
use crate::symbol::Scope;
use crate::symbol::ScopeType;
use crate::util::test::evaluate_test_input_files;
use serde_json::Value;

fn parse_stmt_and_serialize(input: Vec<u8>) -> Value {
  let session = Session::new();
  let mut parser = Parser::new(Lexer::new(&input));
  let scope = Scope::new(&session, None, ScopeType::Global);
  let ctx = ParseCtx {
    scope,
    session: &session,
    rules: ParsePatternRules {
      await_allowed: true,
      yield_allowed: true,
    },
  };
  let node = parser.parse_stmt(ctx).unwrap();
  serde_json::to_value(&node).unwrap()
}

#[test]
fn test_parse_expression() {
  evaluate_test_input_files("parse/tests/stmt", |input| parse_stmt_and_serialize(input));
}
