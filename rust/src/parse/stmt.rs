use super::decl::VarDeclParseMode;
use super::expr::Asi;
use super::pattern::is_valid_pattern_identifier;
use super::ParseCtx;
use super::Parser;
use crate::ast::ExportName;
use crate::ast::ExportNames;
use crate::ast::ForInit;
use crate::ast::Node;
use crate::ast::Syntax;
use crate::error::SyntaxErrorType;
use crate::error::SyntaxResult;
use crate::loc::Loc;
use crate::token::TokenType;

struct BreakOrContinue {
  loc: Loc,
  label: Option<String>,
}

impl<'a> Parser<'a> {
  // Parses `a`, `a as b`, `default as b`. Creates the symbol if importing.
  fn parse_import_or_export_name(&mut self, ctx: ParseCtx) -> SyntaxResult<ExportName> {
    let (target, alias) = match self.consume_if(TokenType::KeywordDefault)?.match_loc() {
      Some(target) => {
        self.require(TokenType::KeywordAs)?;
        let alias = self.require(TokenType::Identifier)?.loc;
        (target, alias)
      }
      None => {
        let target = self.require(TokenType::Identifier)?.loc;
        let alias = if self.consume_if(TokenType::KeywordAs)?.is_match() {
          self.require(TokenType::Identifier)?.loc
        } else {
          target
        };
        (target, alias)
      }
    };
    let alias_node = Node::new(alias, Syntax::IdentifierPattern {
      name: self.string(alias),
    });
    Ok(ExportName {
      target: self.string(target),
      alias: alias_node,
    })
  }

  pub fn parse_stmt(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    match self.peek()?.typ {
      TokenType::BraceOpen => self.parse_stmt_block(ctx),
      TokenType::KeywordBreak => self.parse_stmt_break(ctx),
      TokenType::KeywordClass => self.parse_decl_class(ctx, false, false),
      TokenType::KeywordConst | TokenType::KeywordLet | TokenType::KeywordVar => {
        self.parse_decl_var(ctx, VarDeclParseMode::Asi, false)
      }
      TokenType::KeywordContinue => self.parse_stmt_continue(ctx),
      TokenType::KeywordDebugger => self.parse_stmt_debugger(ctx),
      TokenType::KeywordDo => self.parse_stmt_do_while(ctx),
      TokenType::KeywordExport => self.parse_stmt_export(ctx),
      TokenType::KeywordFor => self.parse_stmt_for(ctx),
      TokenType::KeywordAsync | TokenType::KeywordFunction => {
        self.parse_decl_function(ctx, false, false)
      }
      TokenType::KeywordIf => self.parse_stmt_if(ctx),
      TokenType::KeywordImport => self.parse_stmt_import_or_expr_import(ctx),
      TokenType::KeywordReturn => self.parse_stmt_return(ctx),
      TokenType::KeywordSwitch => self.parse_stmt_switch(ctx),
      TokenType::KeywordThrow => self.parse_stmt_throw(ctx),
      TokenType::KeywordTry => self.parse_stmt_try(ctx),
      TokenType::KeywordWhile => self.parse_stmt_while(ctx),
      TokenType::Semicolon => self.parse_stmt_empty(ctx),
      t if is_valid_pattern_identifier(t, ctx.rules) => {
        let checkpoint = self.checkpoint();
        let label_name = self.next()?.loc;
        if self.consume_if(TokenType::Colon)?.is_match() {
          let statement = self.parse_stmt(ctx)?;
          Ok(Node::new(
            self.since_checkpoint(checkpoint),
            Syntax::LabelStmt {
              name: self.string(label_name),
              statement,
            },
          ))
        } else {
          self.restore_checkpoint(checkpoint);
          self.parse_stmt_expression(ctx)
        }
      }
      _ => self.parse_stmt_expression(ctx),
    }
  }

  pub fn parse_stmt_empty(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let loc = self.require(TokenType::Semicolon)?.loc;
    Ok(Node::new(loc, Syntax::EmptyStmt {}))
  }

  // The scope can be provided when parsing function bodies, as their scope starts at the signature and a new one isn't introduced upon braces.
  pub fn parse_stmt_block_with_existing_scope(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::BraceOpen)?;
    let mut body = Vec::new();
    loop {
      if let Some(end_loc) = self.consume_if(TokenType::BraceClose)?.match_loc() {
        return Ok(Node::new(start.loc + end_loc, Syntax::BlockStmt { body }));
      };
      body.push(self.parse_stmt(ctx)?);
    }
  }

  pub fn parse_stmt_block(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    self.parse_stmt_block_with_existing_scope(ctx)
  }

  fn parse_stmt_break_or_continue(
    &mut self,
    ctx: ParseCtx,
    t: TokenType,
  ) -> SyntaxResult<BreakOrContinue> {
    let mut loc = self.require(t)?.loc;
    let next = self.peek()?;
    let label =
      if is_valid_pattern_identifier(next.typ, ctx.rules) && !next.preceded_by_line_terminator {
        // Label.
        self.consume_peeked();
        loc.extend(next.loc);
        Some(self.string(next.loc))
      } else if next.typ == TokenType::Semicolon {
        self.consume_peeked();
        None
      } else if next.preceded_by_line_terminator || next.typ == TokenType::BraceClose {
        // ASI.
        None
      } else {
        return Err(next.error(SyntaxErrorType::ExpectedSyntax("continue label")));
      };
    Ok(BreakOrContinue { loc, label })
  }

  pub fn parse_stmt_break(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let stmt = self.parse_stmt_break_or_continue(ctx, TokenType::KeywordBreak)?;
    Ok(Node::new(stmt.loc, Syntax::BreakStmt { label: stmt.label }))
  }

  pub fn parse_stmt_continue(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let stmt = self.parse_stmt_break_or_continue(ctx, TokenType::KeywordContinue)?;
    Ok(Node::new(stmt.loc, Syntax::ContinueStmt {
      label: stmt.label,
    }))
  }

  pub fn parse_stmt_debugger(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let loc = self.require(TokenType::KeywordDebugger)?.loc;
    Ok(Node::new(loc, Syntax::DebuggerStmt {}))
  }

  // https://tc39.es/ecma262/#sec-exports
  // https://jakearchibald.com/2021/export-default-thing-vs-thing-as-default/
  pub fn parse_stmt_export(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    // TODO Ensure top-level.
    let start = self.require(TokenType::KeywordExport)?;
    let cp = self.checkpoint();
    let t = self.next()?;
    Ok(match t.typ {
      TokenType::BraceOpen => {
        let mut names = Vec::<ExportName>::new();
        loop {
          if self.consume_if(TokenType::BraceClose)?.is_match() {
            break;
          };
          let name = self.parse_import_or_export_name(ctx)?;
          names.push(name);
          if !self.consume_if(TokenType::Comma)?.is_match() {
            self.require(TokenType::BraceClose)?;
            break;
          };
        }
        let from = self.consume_if(TokenType::KeywordFrom)?.and_then(|| {
          let from = self.parse_and_normalise_literal_string(ctx)?;
          Ok(from)
        })?;
        // TODO Loc
        Node::new(start.loc, Syntax::ExportListStmt {
          names: ExportNames::Specific(names),
          from,
        })
      }
      TokenType::Asterisk => {
        let alias = if self.consume_if(TokenType::KeywordAs)?.is_match() {
          let alias = self.require(TokenType::Identifier)?.loc;
          let alias_node = Node::new(alias, Syntax::IdentifierPattern {
            name: self.string(alias),
          });
          Some(alias_node)
          // We don't need to add the symbol as it's not exposed within the module's scope.
        } else {
          None
        };
        self.require(TokenType::KeywordFrom)?;
        let from = self.parse_and_normalise_literal_string(ctx)?;
        // TODO Loc
        Node::new(start.loc, Syntax::ExportListStmt {
          names: ExportNames::All(alias),
          from: Some(from),
        })
      }
      TokenType::KeywordDefault => match self.peek()?.typ {
        // `class` and `function` are treated as statements that are hoisted, not expressions; however, they can be unnamed, which gives them the name `default`.
        TokenType::KeywordAsync | TokenType::KeywordFunction => {
          self.parse_decl_function(ctx, true, true)?
        }
        TokenType::KeywordClass => self.parse_decl_class(ctx, true, true)?,
        _ => {
          let expression = self.parse_expr(ctx, TokenType::Semicolon)?;
          Node::new(start.loc + expression.loc, Syntax::ExportDefaultExprStmt {
            expression,
          })
        }
      },
      TokenType::KeywordVar | TokenType::KeywordLet | TokenType::KeywordConst => {
        // Reconsume declaration keyword.
        self.restore_checkpoint(cp);
        self.parse_decl_var(ctx, VarDeclParseMode::Asi, true)?
      }
      TokenType::KeywordFunction => {
        // Reconsume declaration keyword.
        self.restore_checkpoint(cp);
        self.parse_decl_function(ctx, true, false)?
      }
      TokenType::KeywordClass => {
        // Reconsume declaration keyword.
        self.restore_checkpoint(cp);
        self.parse_decl_class(ctx, true, false)?
      }
      _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("exportable"))),
    })
  }

  // WARNING: Do not reuse this functions for other statements, as this will output a statement node, not an expression, which can lead to double semicolons that cause invalid code when outputting.
  pub fn parse_stmt_expression(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let mut asi = Asi::can();
    let expression = self.parse_expr_with_asi(ctx, TokenType::Semicolon, &mut asi)?;
    if !asi.did_end_with_asi {
      self.require(TokenType::Semicolon)?;
    };
    Ok(Node::new(expression.loc, Syntax::ExpressionStmt {
      expression,
    }))
  }

  pub fn parse_stmt_for(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordFor)?;
    let await_ = self.consume_if(TokenType::KeywordAwait)?;
    self.require(TokenType::ParenthesisOpen)?;
    enum LhsRaw {
      Declaration(Node),
      Expression(Node),
      Pattern(Node),
      Empty,
    }
    let lhs_raw = match self.peek()?.typ {
      TokenType::KeywordVar | TokenType::KeywordLet | TokenType::KeywordConst => {
        LhsRaw::Declaration(self.parse_decl_var(ctx, VarDeclParseMode::Leftmost, false)?)
      }
      TokenType::Semicolon => LhsRaw::Empty,
      _ => {
        // A pattern could be reinterpreted as an expression (and vice versa), so we can only try parsing both.
        let checkpoint = self.checkpoint();
        match if let Ok(node) = self.parse_pattern(ctx) {
          match self.peek()?.typ {
            TokenType::KeywordIn | TokenType::KeywordOf => Some(LhsRaw::Pattern(node)),
            _ => {
              // Mistakenly interpreted as pattern.
              None
            }
          }
        } else {
          None
        } {
          Some(p) => p,
          None => {
            self.restore_checkpoint(checkpoint);
            LhsRaw::Expression(self.parse_expr(ctx, TokenType::Semicolon)?)
          }
        }
      }
    };
    let n = match self.peek()?.typ {
      TokenType::KeywordOf | TokenType::KeywordIn => {
        // for-of or for-in statement.
        let of = match self.next()?.typ {
          TokenType::KeywordOf => true,
          TokenType::KeywordIn => false,
          _ => unreachable!(),
        };
        if !of && await_.is_match() {
          // A for-in statement cannot have await.
          return Err(await_.error(SyntaxErrorType::RequiredTokenNotFound(
            TokenType::ParenthesisOpen,
          )));
        };
        let (decl_mode, pat) = match lhs_raw {
          LhsRaw::Empty => return Err(start.error(SyntaxErrorType::ForLoopHeaderHasNoLhs)),
          LhsRaw::Declaration(node) => match *node.stx {
            Syntax::VarDecl {
              mut declarators,
              mode,
              export,
            } => {
              if export {
                return Err(start.error(SyntaxErrorType::ForLoopHeaderHasInvalidLhs));
              };
              if declarators.len() != 1 {
                return Err(start.error(SyntaxErrorType::ForLoopHeaderHasMultipleDeclarators));
              }
              let decl = declarators.pop().unwrap();
              if decl.initializer.is_some() {
                return Err(start.error(SyntaxErrorType::ForLoopHeaderHasInvalidLhs));
              };
              (Some(mode), decl.pattern)
            }
            _ => unreachable!(),
          },
          LhsRaw::Pattern(pat) => (None, pat),
          LhsRaw::Expression(_) => {
            return Err(start.error(SyntaxErrorType::ForLoopHeaderHasInvalidLhs))
          }
        };
        let rhs = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
        self.require(TokenType::ParenthesisClose)?;
        let body = self.parse_stmt(ctx)?;
        if of {
          Node::new(start.loc + body.loc, Syntax::ForOfStmt {
            await_: await_.is_match(),
            decl_mode,
            pat,
            rhs,
            body,
          })
        } else {
          Node::new(start.loc + body.loc, Syntax::ForInStmt {
            decl_mode,
            pat,
            rhs,
            body,
          })
        }
      }
      _ => {
        // for statement.
        if await_.is_match() {
          // A for statement cannot have await.
          return Err(await_.error(SyntaxErrorType::RequiredTokenNotFound(
            TokenType::ParenthesisOpen,
          )));
        }
        let init = match lhs_raw {
          LhsRaw::Declaration(decl) => {
            self.require(TokenType::Semicolon)?;
            ForInit::Declaration(decl)
          }
          LhsRaw::Expression(expr) => {
            // We must check, due to possibility of illegal ASI (see previous).
            self.require(TokenType::Semicolon)?;
            ForInit::Expression(expr)
          }
          LhsRaw::Empty => {
            self.require(TokenType::Semicolon)?;
            ForInit::None
          }
          LhsRaw::Pattern(_) => {
            return Err(start.error(SyntaxErrorType::ForLoopHeaderHasInvalidLhs))
          }
        };
        let condition = if self.consume_if(TokenType::Semicolon)?.is_match() {
          None
        } else {
          let expr = self.parse_expr(ctx, TokenType::Semicolon)?;
          self.require(TokenType::Semicolon)?;
          Some(expr)
        };
        let post = if self.consume_if(TokenType::ParenthesisClose)?.is_match() {
          None
        } else {
          let expr = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
          self.require(TokenType::ParenthesisClose)?;
          Some(expr)
        };
        let body = self.parse_stmt(ctx)?;
        Node::new(start.loc + body.loc, Syntax::ForStmt {
          init,
          condition,
          post,
          body,
        })
      }
    };
    Ok(n)
  }

  pub fn parse_stmt_if(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordIf)?;
    self.require(TokenType::ParenthesisOpen)?;
    let test = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
    self.require(TokenType::ParenthesisClose)?;
    let consequent = self.parse_stmt(ctx)?;
    let alternate = if self.consume_if(TokenType::KeywordElse)?.is_match() {
      Some(self.parse_stmt(ctx)?)
    } else {
      None
    };
    let end = alternate.as_ref().unwrap_or(&consequent);

    Ok(Node::new(start.loc + end.loc, Syntax::IfStmt {
      test,
      consequent,
      alternate,
    }))
  }

  pub fn parse_stmt_import_or_expr_import(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let cp = self.checkpoint();
    let start = self.require(TokenType::KeywordImport)?;
    if self.consume_if(TokenType::ParenthesisOpen)?.is_match() {
      self.restore_checkpoint(cp);
      return self.parse_stmt_expression(ctx);
    };

    // TODO Ensure top-level.

    let (default, can_have_names) =
      if let Some(alias) = self.consume_if(TokenType::Identifier)?.match_loc() {
        let alias_node = Node::new(alias, Syntax::IdentifierPattern {
          name: self.string(alias),
        });
        (
          Some(alias_node),
          self.consume_if(TokenType::Comma)?.is_match(),
        )
      } else {
        (None, true)
      };
    let names = if !can_have_names {
      None
    } else if self.consume_if(TokenType::Asterisk)?.is_match() {
      self.require(TokenType::KeywordAs)?;
      let alias = self.require(TokenType::Identifier)?.loc;
      let alias_node = Node::new(alias, Syntax::IdentifierPattern {
        name: self.string(alias),
      });
      Some(ExportNames::All(Some(alias_node)))
    } else {
      self.require(TokenType::BraceOpen)?;
      let mut names = Vec::<ExportName>::new();
      while !self.consume_if(TokenType::BraceClose)?.is_match() {
        let name = self.parse_import_or_export_name(ctx)?;
        names.push(name);
        if !self.consume_if(TokenType::Comma)?.is_match() {
          break;
        };
      }
      self.require(TokenType::BraceClose)?;
      Some(ExportNames::Specific(names))
    };
    self.require(TokenType::KeywordFrom)?;
    let module = self.parse_and_normalise_literal_string(ctx)?;
    self.require(TokenType::Semicolon)?;
    // TODO Loc
    Ok(Node::new(start.loc, Syntax::ImportStmt {
      default,
      module,
      names,
    }))
  }

  pub fn parse_stmt_return(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordReturn)?;
    let mut loc = start.loc;
    let value =
      if self.peek()?.preceded_by_line_terminator || self.peek()?.typ == TokenType::BraceClose {
        // Automatic Semicolon Insertion.
        None
      } else if self.consume_if(TokenType::Semicolon)?.is_match() {
        None
      } else {
        let mut asi = Asi::can();
        let value = self.parse_expr_with_asi(ctx, TokenType::Semicolon, &mut asi)?;
        if !asi.did_end_with_asi {
          self.require(TokenType::Semicolon)?;
        };
        loc.extend(value.loc);
        Some(value)
      };
    Ok(Node::new(loc, Syntax::ReturnStmt { value }))
  }

  pub fn parse_stmt_throw(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordThrow)?;
    if self.peek()?.preceded_by_line_terminator {
      // Illegal under Automatic Semicolon Insertion rules.
      return Err(start.error(SyntaxErrorType::LineTerminatorAfterThrow));
    }
    let mut asi = Asi::can();
    let value = self.parse_expr_with_asi(ctx, TokenType::Semicolon, &mut asi)?;
    if !asi.did_end_with_asi {
      self.require(TokenType::Semicolon)?;
    };
    Ok(Node::new(start.loc + value.loc, Syntax::ThrowStmt {
      value,
    }))
  }

  pub fn parse_stmt_try(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordTry)?;
    let mut loc = start.loc;
    let wrapped = self.parse_stmt_block(ctx)?;
    let catch = if self.consume_if(TokenType::KeywordCatch)?.is_match() {
      let parameter = if self.consume_if(TokenType::ParenthesisOpen)?.is_match() {
        let pattern = self.parse_pattern(ctx)?;
        self.require(TokenType::ParenthesisClose)?;
        Some(pattern)
      } else {
        None
      };
      let body = self.parse_stmt_block(ctx)?;
      loc.extend(body.loc);
      Some(Node::new(body.loc, Syntax::CatchBlock { parameter, body }))
    } else {
      None
    };
    let finally = if self.consume_if(TokenType::KeywordFinally)?.is_match() {
      let body = self.parse_stmt_block(ctx)?;
      loc.extend(body.loc);
      Some(body)
    } else {
      None
    };
    if catch.is_none() && finally.is_none() {
      return Err(start.error(SyntaxErrorType::TryStatementHasNoCatchOrFinally));
    }
    Ok(Node::new(loc, Syntax::TryStmt {
      wrapped,
      catch,
      finally,
    }))
  }

  pub fn parse_stmt_while(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordWhile)?;
    self.require(TokenType::ParenthesisOpen)?;
    let condition = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
    self.require(TokenType::ParenthesisClose)?;
    let body = self.parse_stmt(ctx)?;
    Ok(Node::new(start.loc + body.loc, Syntax::WhileStmt {
      condition,
      body,
    }))
  }

  pub fn parse_stmt_do_while(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordDo)?;
    let body = self.parse_stmt(ctx)?;
    self.require(TokenType::KeywordWhile)?;
    self.require(TokenType::ParenthesisOpen)?;
    let condition = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
    let end = self.require(TokenType::ParenthesisClose)?;
    self.consume_if(TokenType::Semicolon)?;
    Ok(Node::new(start.loc + end.loc, Syntax::DoWhileStmt {
      condition,
      body,
    }))
  }

  pub fn parse_stmt_switch(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordSwitch)?;
    self.require(TokenType::ParenthesisOpen)?;
    let test = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
    self.require(TokenType::ParenthesisClose)?;
    self.require(TokenType::BraceOpen)?;
    let mut branches = Vec::new();
    while self.peek()?.typ != TokenType::BraceClose {
      let mut loc = self.peek()?.loc;
      let case = if self.consume_if(TokenType::KeywordCase)?.is_match() {
        Some(self.parse_expr(ctx, TokenType::Colon)?)
      } else {
        self.require(TokenType::KeywordDefault)?;
        None
      };
      self.require(TokenType::Colon)?;
      let mut body = Vec::new();
      loop {
        match self.peek()?.typ {
          TokenType::KeywordCase | TokenType::KeywordDefault | TokenType::BraceClose => break,
          _ => {
            let stmt = self.parse_stmt(ctx)?;
            loc.extend(stmt.loc);
            body.push(stmt);
          }
        }
      }
      branches.push(Node::new(loc, Syntax::SwitchBranch { case, body }));
    }
    let end = self.require(TokenType::BraceClose)?;
    Ok(Node::new(start.loc + end.loc, Syntax::SwitchStmt {
      test,
      branches,
    }))
  }
}
