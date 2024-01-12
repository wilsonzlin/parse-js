use super::class_or_object::ParseClassBodyResult;
use super::expr::Asi;
use super::pattern::is_valid_pattern_identifier;
use super::pattern::ParsePatternRules;
use super::ParseCtx;
use super::Parser;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::ast::VarDeclMode;
use crate::ast::VariableDeclarator;
use crate::error::SyntaxErrorType;
use crate::error::SyntaxResult;
use crate::token::TokenType;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum VarDeclParseMode {
  // Standard parsing mode for var/let/const statement.
  Asi,
  // Parse as many valid declarators as possible, then break before the first invalid token (i.e. not a comma). Used by for-loop parser.
  Leftmost,
}

impl<'a> Parser<'a> {
  pub fn parse_decl_var(
    &mut self,
    ctx: ParseCtx,
    parse_mode: VarDeclParseMode,
    export: bool,
  ) -> SyntaxResult<Node> {
    let t = self.next()?;
    let mode = match t.typ {
      TokenType::KeywordLet => VarDeclMode::Let,
      TokenType::KeywordConst => VarDeclMode::Const,
      TokenType::KeywordVar => VarDeclMode::Var,
      _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("variable declaration"))),
    };
    let mut declarators = Vec::new();
    let mut loc = t.loc;
    loop {
      let pattern = self.parse_pattern(ctx)?;
      loc.extend(pattern.loc);
      let mut asi = match parse_mode {
        VarDeclParseMode::Asi => Asi::can(),
        VarDeclParseMode::Leftmost => Asi::no(),
      };
      let initializer = if self.consume_if(TokenType::Equals)?.is_match() {
        let expr = self.parse_expr_until_either_with_asi(
          ctx,
          TokenType::Semicolon,
          TokenType::Comma,
          &mut asi,
        )?;
        loc.extend(expr.loc);
        Some(expr)
      } else {
        None
      };
      declarators.push(VariableDeclarator {
        pattern,
        initializer,
      });
      match parse_mode {
        VarDeclParseMode::Asi => {
          if self.consume_if(TokenType::Semicolon)?.is_match() || asi.did_end_with_asi {
            break;
          }
          let t = self.peek()?;
          if t.preceded_by_line_terminator && t.typ != TokenType::Comma {
            break;
          };
          self.require(TokenType::Comma)?;
        }
        VarDeclParseMode::Leftmost => {
          if !self.consume_if(TokenType::Comma)?.is_match() {
            break;
          }
        }
      }
    }
    Ok(Node::new(loc, Syntax::VarDecl {
      export,
      mode,
      declarators,
    }))
  }

  pub fn parse_decl_function(
    &mut self,
    ctx: ParseCtx,
    export: bool,
    export_default: bool,
  ) -> SyntaxResult<Node> {
    let is_async = self.consume_if(TokenType::KeywordAsync)?.is_match();
    let start = self.require(TokenType::KeywordFunction)?.loc;
    let generator = self.consume_if(TokenType::Asterisk)?.is_match();
    // WARNING: The name belongs in the containing scope, not the function's scope.
    // For example, `function a() { let a = 1; }` is legal.
    // The name can only be omitted in default exports.
    let name = match self
      .consume_if_pred(|t| is_valid_pattern_identifier(t.typ, ctx.rules))?
      .match_loc()
    {
      Some(name) => {
        let name_node = Node::new(name, Syntax::ClassOrFunctionName {
          name: self.string(name),
        });
        Some(name_node)
      }
      _ => {
        if !export_default {
          return Err(start.error(SyntaxErrorType::ExpectedSyntax("function name"), None));
        };
        None
      }
    };
    let signature = self.parse_signature_function(ctx)?;
    let body = self.parse_stmt_block(ctx.with_rules(ParsePatternRules {
      await_allowed: !is_async && ctx.rules.await_allowed,
      yield_allowed: !generator && ctx.rules.yield_allowed,
    }))?;
    Ok(Node::new(start + body.loc, Syntax::FunctionDecl {
      export,
      export_default,
      is_async,
      generator,
      name,
      signature,
      body,
    }))
  }

  pub fn parse_decl_class(
    &mut self,
    ctx: ParseCtx,
    export: bool,
    export_default: bool,
  ) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordClass)?.loc;
    // Names can be omitted only in default exports.
    let name = match self
      .consume_if_pred(|t| is_valid_pattern_identifier(t.typ, ctx.rules))?
      .match_loc()
    {
      Some(name) => {
        let name_node = Node::new(name, Syntax::ClassOrFunctionName {
          name: self.string(name),
        });
        Some(name_node)
      }
      None => {
        if !export_default {
          return Err(start.error(SyntaxErrorType::ExpectedSyntax("class name"), None));
        };
        None
      }
    };
    // Unlike functions, classes are scoped to their block.
    let extends = if self.consume_if(TokenType::KeywordExtends)?.is_match() {
      Some(self.parse_expr(ctx, TokenType::BraceOpen)?)
    } else {
      None
    };
    let ParseClassBodyResult { end, members } = self.parse_class_body(ctx)?;
    Ok(Node::new(start + end, Syntax::ClassDecl {
      export,
      export_default,
      name,
      extends,
      members,
    }))
  }
}
