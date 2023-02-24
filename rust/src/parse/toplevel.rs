use super::pattern::ParsePatternRules;
use super::ParseCtx;
use super::Parser;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxResult;
use crate::session::Session;
use crate::symbol::Scope;
use crate::symbol::ScopeType;
use crate::symbol::SymbolGenerator;
use crate::token::TokenType;
use core::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TopLevelMode {
  Global,
  Module,
}

impl FromStr for TopLevelMode {
  type Err = &'static str;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "global" => Ok(TopLevelMode::Global),
      "module" => Ok(TopLevelMode::Module),
      _ => Err("invalid top-level mode"),
    }
  }
}

impl<'a> Parser<'a> {
  pub fn parse_top_level(
    &mut self,
    session: &'a Session,
    symbol_generator: SymbolGenerator,
    top_level_mode: TopLevelMode,
  ) -> SyntaxResult<'a, Node<'a>> {
    let ctx = ParseCtx {
      scope: Scope::new(session, symbol_generator, None, match top_level_mode {
        TopLevelMode::Global => ScopeType::Global,
        TopLevelMode::Module => ScopeType::Module,
      }),
      session,
      rules: ParsePatternRules {
        await_allowed: true,
        yield_allowed: true,
      },
    };
    let mut body = ctx.session.new_vec();
    while !self.consume_if(TokenType::EOF)?.is_match() {
      body.push(self.parse_stmt(ctx)?);
    }
    let top_level_node = ctx.create_node(self.source_range(), Syntax::TopLevel { body });
    Ok(top_level_node)
  }
}
