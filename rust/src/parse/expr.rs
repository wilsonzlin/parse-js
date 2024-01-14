use super::class_or_object::ParseClassBodyResult;
use super::class_or_object::ParseClassOrObjectMemberResult;
use super::literal::normalise_literal_bigint;
use super::literal::normalise_literal_string_or_template_inner;
use super::pattern::is_valid_pattern_identifier;
use super::pattern::ParsePatternRules;
use super::ParseCtx;
use super::Parser;
use crate::ast::ArrayElement;
use crate::ast::ArrayPatternElement;
use crate::ast::ClassOrObjectMemberKey;
use crate::ast::ClassOrObjectMemberValue;
use crate::ast::LiteralTemplatePart;
use crate::ast::Node;
use crate::ast::ObjectMemberType;
use crate::ast::Syntax;
use crate::error::SyntaxErrorType;
use crate::error::SyntaxResult;
use crate::lex::lex_template_string_continue;
use crate::lex::LexMode;
use crate::lex::KEYWORDS_MAPPING;
use crate::operator::Associativity;
use crate::operator::OperatorName;
use crate::operator::OPERATORS;
use crate::parse::literal::normalise_literal_number;
use crate::parse::literal::normalise_literal_string;
use crate::parse::operator::MULTARY_OPERATOR_MAPPING;
use crate::parse::operator::UNARY_OPERATOR_MAPPING;
use crate::token::TokenType;

pub struct Asi {
  pub can_end_with_asi: bool,
  pub did_end_with_asi: bool,
}

impl Asi {
  pub fn can() -> Asi {
    Asi {
      can_end_with_asi: true,
      did_end_with_asi: false,
    }
  }

  pub fn no() -> Asi {
    Asi {
      can_end_with_asi: false,
      did_end_with_asi: false,
    }
  }
}

fn is_chevron_right_or_slash(typ: TokenType) -> bool {
  typ == TokenType::ChevronRight || typ == TokenType::Slash
}

fn jsx_tag_names_are_equal(a: Option<&Syntax>, b: Option<&Syntax>) -> bool {
  let (Some(a), Some(b)) = (a, b) else {
    return a.is_none() && b.is_none();
  };
  match (a, b) {
    (
      Syntax::JsxMemberExpression {
        base: a_base,
        path: a_path,
      },
      Syntax::JsxMemberExpression {
        base: b_base,
        path: b_path,
      },
    ) => a_base.as_ident() == b_base.as_ident() && a_path == b_path,
    (
      Syntax::JsxName {
        name: a_name,
        namespace: a_ns,
      },
      Syntax::JsxName {
        name: b_name,
        namespace: b_ns,
      },
    ) => a_ns == b_ns && a_name == b_name,
    (Syntax::IdentifierExpr { name: a_name }, Syntax::IdentifierExpr { name: b_name }) => {
      a_name == b_name
    }
    _ => false,
  }
}

impl<'a> Parser<'a> {
  /// Reinterprets an expression subtree as an assignment target.
  fn transform_literal_expr_to_destructuring_pattern(
    &self,
    ctx: ParseCtx,
    node: Node,
  ) -> SyntaxResult<Node> {
    let loc = node.loc;
    match *node.stx {
      Syntax::LiteralArrayExpr { elements } => {
        let mut pat_elements = Vec::<Option<ArrayPatternElement>>::new();
        let mut rest = None;
        for element in elements {
          if rest.is_some() {
            return Err(loc.error(SyntaxErrorType::InvalidAssigmentTarget, None));
          };
          match element {
            ArrayElement::Single(elem) => {
              match *elem.stx {
                Syntax::BinaryExpr {
                  parenthesised,
                  operator,
                  left,
                  right,
                } => {
                  if parenthesised || operator != OperatorName::Assignment {
                    return Err(loc.error(SyntaxErrorType::InvalidAssigmentTarget, None));
                  };
                  pat_elements.push(Some(ArrayPatternElement {
                    target: self.transform_literal_expr_to_destructuring_pattern(ctx, left)?,
                    default_value: Some(right),
                  }));
                }
                _ => pat_elements.push(Some(ArrayPatternElement {
                  target: self.transform_literal_expr_to_destructuring_pattern(ctx, elem)?,
                  default_value: None,
                })),
              };
            }
            ArrayElement::Rest(expr) => {
              rest = Some(self.transform_literal_expr_to_destructuring_pattern(ctx, expr)?);
            }
            ArrayElement::Empty => pat_elements.push(None),
          };
        }
        Ok(Node::new(loc, Syntax::ArrayPattern {
          elements: pat_elements,
          rest,
        }))
      }
      Syntax::LiteralObjectExpr { members } => {
        let mut properties = Vec::new();
        let mut rest = None;
        for member in members {
          if rest.is_some() {
            return Err(loc.error(SyntaxErrorType::InvalidAssigmentTarget, None));
          };
          match *member.stx {
            Syntax::ObjectMember { typ } => match typ {
              ObjectMemberType::Valued { key, value } => {
                let (target, default_value) = match value {
                  ClassOrObjectMemberValue::Property {
                    initializer: Some(initializer),
                  } => match *initializer.stx {
                    Syntax::BinaryExpr {
                      parenthesised,
                      operator,
                      left,
                      right,
                    } => {
                      if parenthesised || operator != OperatorName::Assignment {
                        return Err(loc.error(SyntaxErrorType::InvalidAssigmentTarget, None));
                      };
                      (
                        self.transform_literal_expr_to_destructuring_pattern(ctx, left)?,
                        Some(right),
                      )
                    }
                    _ => (
                      self.transform_literal_expr_to_destructuring_pattern(ctx, initializer)?,
                      None,
                    ),
                  },
                  _ => return Err(loc.error(SyntaxErrorType::InvalidAssigmentTarget, None)),
                };
                properties.push(Node::new(loc, Syntax::ObjectPatternProperty {
                  key,
                  target,
                  default_value,
                  shorthand: true,
                }));
              }
              ObjectMemberType::Shorthand { identifier } => {
                properties.push(Node::new(loc, Syntax::ObjectPatternProperty {
                  key: ClassOrObjectMemberKey::Direct(self.string(identifier.loc)),
                  target: Node::new(loc, Syntax::IdentifierPattern {
                    name: self.string(identifier.loc),
                  }),
                  default_value: None,
                  shorthand: true,
                }));
              }
              ObjectMemberType::Rest { value } => {
                rest = Some(self.transform_literal_expr_to_destructuring_pattern(ctx, value)?);
              }
            },
            _ => unreachable!(),
          };
        }
        Ok(Node::new(loc, Syntax::ObjectPattern { properties, rest }))
      }
      // It's possible to encounter an IdentifierPattern e.g. `{ a: b = 1 } = x`, where `b = 1` is already parsed as an assignment.
      Syntax::IdentifierExpr { name } | Syntax::IdentifierPattern { name } => {
        Ok(Node::new(loc, Syntax::IdentifierPattern {
          name: name.clone(),
        }))
      }
      _ => Err(loc.error(SyntaxErrorType::InvalidAssigmentTarget, None)),
    }
  }

  // Trying to check if every object, array, or identifier expression operand is actually an assignment target first is too expensive and wasteful, so simply retroactively transform the LHS of a BinaryExpr with Assignment* operator into a target, raising an error if it can't (and is an invalid assignment target). A valid target is:
  // - A chain of non-optional-chaining member, computed member, and call operators, not ending in a call.
  // - A pattern.
  fn convert_assignment_lhs_to_target(
    &self,
    ctx: ParseCtx,
    lhs: Node,
    operator_name: OperatorName,
  ) -> SyntaxResult<Node> {
    match lhs.stx.as_ref() {
      e @ (Syntax::LiteralArrayExpr { .. }
      | Syntax::LiteralObjectExpr { .. }
      | Syntax::IdentifierExpr { .. }) => {
        if operator_name != OperatorName::Assignment
          && match e {
            Syntax::IdentifierExpr { .. } => false,
            _ => true,
          }
        {
          return Err(lhs.error(SyntaxErrorType::InvalidAssigmentTarget));
        }
        // We must transform into a pattern.
        let root = self.transform_literal_expr_to_destructuring_pattern(ctx, lhs)?;
        Ok(root)
      }
      Syntax::ComputedMemberExpr {
        optional_chaining, ..
      }
      | Syntax::MemberExpr {
        optional_chaining, ..
      } if !optional_chaining => {
        // As long as the expression ends with ComputedMemberExpr or MemberExpr, it's valid e.g. `(a, b?.a ?? 3, c = d || {})[1] = x`. Note that this is after parsing, so `a + b.c = 3` is invalid because that parses to `(a + b.c) = 3`, with a LHS of BinaryExpr with Addition operator.
        // TODO Technically there cannot be any optional chaining in the entire access/call path, not just in the last part (e.g. `a.b?.c.d = e` is invalid).
        Ok(lhs)
      }
      _ => Err(lhs.error(SyntaxErrorType::InvalidAssigmentTarget)),
    }
  }

  pub fn parse_jsx_name(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require_with_mode(TokenType::Identifier, LexMode::JsxTag)?;
    Ok(if self.consume_if(TokenType::Colon)?.is_match() {
      let name = self.require_with_mode(TokenType::Identifier, LexMode::JsxTag)?;
      Node::new(start.loc + name.loc, Syntax::JsxName {
        namespace: Some(self.string(start.loc)),
        name: self.string(name.loc),
      })
    } else {
      Node::new(start.loc, Syntax::JsxName {
        namespace: None,
        name: self.string(start.loc),
      })
    })
  }

  pub fn parse_jsx_tag_name(&mut self, ctx: ParseCtx) -> SyntaxResult<Option<Node>> {
    Ok(
      match self
        .maybe_with_mode(TokenType::Identifier, LexMode::JsxTag)?
        .match_loc()
      {
        // Fragment.
        None => None,
        Some(start) => Some({
          if self.consume_if(TokenType::Colon)?.is_match() {
            // Namespaced name.
            let name = self.require_with_mode(TokenType::Identifier, LexMode::JsxTag)?;
            Node::new(start + name.loc, Syntax::JsxName {
              namespace: Some(self.string(start)),
              name: self.string(name.loc),
            })
          } else if self.peek()?.typ == TokenType::Dot && !self.str(start).contains('-') {
            // Member name.
            let mut path = Vec::new();
            let mut loc = start;
            while self.consume_if(TokenType::Dot)?.is_match() {
              let l = self.require(TokenType::Identifier)?.loc;
              path.push(self.string(l));
              loc += l;
            }
            Node::new(loc, Syntax::JsxMemberExpression {
              base: Node::new(start, Syntax::IdentifierExpr {
                name: self.string(start),
              }),
              path,
            })
          } else if !self.bytes(start)[0].is_ascii_lowercase() {
            // User-defined component.
            Node::new(start, Syntax::IdentifierExpr {
              name: self.string(start),
            })
          } else {
            // Built-in component without namespace.
            Node::new(start, Syntax::JsxName {
              namespace: None,
              name: self.string(start),
            })
          }
        }),
      },
    )
  }

  // https://facebook.github.io/jsx/
  pub fn parse_jsx_element(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let tag_start = self.require(TokenType::ChevronLeft)?;
    let tag_name = self.parse_jsx_tag_name(ctx)?;

    // Attributes.
    let mut attributes = Vec::new();
    if tag_name.is_some() {
      loop {
        if is_chevron_right_or_slash(self.peek()?.typ) {
          break;
        }
        if self.consume_if(TokenType::BraceOpen)?.is_match() {
          let start = self.require(TokenType::DotDotDot)?;
          let value = self.parse_expr(ctx, TokenType::BraceClose)?;
          let end = self.require(TokenType::BraceClose)?;
          attributes.push(Node::new(start.loc + end.loc, Syntax::JsxSpreadAttribute {
            value,
          }));
          continue;
        }

        let name = self.parse_jsx_name(ctx)?;
        let value = if !self.consume_if(TokenType::Equals)?.is_match() {
          None
        } else {
          // TODO JSXSpreadAttribute
          // TODO Attr values can be an element or fragment directly e.g. `a=<div/>`.
          Some(if self.consume_if(TokenType::BraceOpen)?.is_match() {
            let value = self.parse_expr(ctx, TokenType::BraceClose)?;
            let expr = Node::new(value.loc, Syntax::JsxExpressionContainer { value });
            self.require(TokenType::BraceClose)?;
            expr
          } else {
            let value = self.require(TokenType::LiteralString)?;
            Node::new(value.loc, Syntax::JsxText {
              value: self.string(value.loc),
            })
          })
        };
        attributes.push(Node::new(
          name.loc.add_option(value.as_ref().map(|n| n.loc)),
          Syntax::JsxAttribute { name, value },
        ))
      }
    }

    Ok(if self.consume_if(TokenType::Slash)?.is_match() {
      // Self closing.
      let end = self.require(TokenType::ChevronRight)?;
      Node::new(tag_start.loc + end.loc, Syntax::JsxElement {
        name: tag_name,
        attributes,
        children: Vec::new(),
      })
    } else {
      self.require(TokenType::ChevronRight)?;

      // Children.
      let mut children = Vec::new();
      let close_start = loop {
        match self.peek()? {
          t if t.typ == TokenType::ChevronLeftSlash => {
            break self.consume_peeked();
          }
          t if t.typ == TokenType::EOF => {
            return Err(t.error(SyntaxErrorType::UnexpectedEnd));
          }
          _ => {}
        };
        let text = self.require_with_mode(TokenType::JsxTextContent, LexMode::JsxTextContent)?;
        if !text.loc.is_empty() {
          children.push(Node::new(text.loc, Syntax::JsxText {
            value: self.string(text.loc),
          }));
        };
        if self.peek()?.typ == TokenType::ChevronLeft {
          children.push(self.parse_jsx_element(ctx)?);
        };
        if self.consume_if(TokenType::BraceOpen)?.is_match() {
          // TODO Allow empty expr.
          let value = self.parse_expr(ctx, TokenType::BraceClose)?;
          children.push(Node::new(value.loc, Syntax::JsxExpressionContainer {
            value,
          }));
          self.require(TokenType::BraceClose)?;
        };
      };
      let end_name = self.parse_jsx_tag_name(ctx)?;
      if !jsx_tag_names_are_equal(
        tag_name.as_ref().map(|n| n.stx.as_ref()),
        end_name.as_ref().map(|n| n.stx.as_ref()),
      ) {
        return Err(close_start.error(SyntaxErrorType::JsxClosingTagMismatch));
      };
      let end = self.require(TokenType::ChevronRight)?;
      Node::new(tag_start.loc + end.loc, Syntax::JsxElement {
        name: tag_name,
        attributes,
        children,
      })
    })
  }

  pub fn parse_call_args(&mut self, ctx: ParseCtx) -> SyntaxResult<Vec<Node>> {
    let mut args = Vec::new();
    loop {
      if self.peek()?.typ == TokenType::ParenthesisClose {
        break;
      };
      let spread = self.consume_if(TokenType::DotDotDot)?.is_match();
      let value =
        self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::ParenthesisClose)?;
      args.push(Node::new(value.loc, Syntax::CallArg { spread, value }));
      if !self.consume_if(TokenType::Comma)?.is_match() {
        break;
      };
    }
    Ok(args)
  }

  pub fn parse_expr(&mut self, ctx: ParseCtx, terminator: TokenType) -> SyntaxResult<Node> {
    self.parse_expr_with_min_prec(ctx, 1, terminator, TokenType::_Dummy, false, &mut Asi::no())
  }

  pub fn parse_expr_with_asi(
    &mut self,
    ctx: ParseCtx,
    terminator: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<Node> {
    self.parse_expr_with_min_prec(ctx, 1, terminator, TokenType::_Dummy, false, asi)
  }

  pub fn parse_expr_until_either(
    &mut self,
    ctx: ParseCtx,
    terminator_a: TokenType,
    terminator_b: TokenType,
  ) -> SyntaxResult<Node> {
    self.parse_expr_with_min_prec(ctx, 1, terminator_a, terminator_b, false, &mut Asi::no())
  }

  pub fn parse_expr_until_either_with_asi(
    &mut self,
    ctx: ParseCtx,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<Node> {
    self.parse_expr_with_min_prec(ctx, 1, terminator_a, terminator_b, false, asi)
  }

  pub fn parse_grouping(&mut self, ctx: ParseCtx, asi: &mut Asi) -> SyntaxResult<Node> {
    self.require(TokenType::ParenthesisOpen)?;
    let expr = self.parse_expr_with_min_prec(
      ctx,
      1,
      TokenType::ParenthesisClose,
      TokenType::_Dummy,
      true,
      asi,
    )?;
    self.require(TokenType::ParenthesisClose)?;
    Ok(expr)
  }

  pub fn parse_expr_array(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let loc_start = self.require(TokenType::BracketOpen)?.loc;
    let mut elements = Vec::<ArrayElement>::new();
    loop {
      if self.consume_if(TokenType::Comma)?.is_match() {
        elements.push(ArrayElement::Empty);
        continue;
      };
      if self.peek()?.typ == TokenType::BracketClose {
        break;
      };
      let rest = self.consume_if(TokenType::DotDotDot)?.is_match();
      let value = self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::BracketClose)?;
      elements.push(if rest {
        ArrayElement::Rest(value)
      } else {
        ArrayElement::Single(value)
      });
      if self.peek()?.typ == TokenType::BracketClose {
        break;
      };
      self.require(TokenType::Comma)?;
    }
    let loc_end = self.require(TokenType::BracketClose)?.loc;
    Ok(Node::new(loc_start + loc_end, Syntax::LiteralArrayExpr {
      elements,
    }))
  }

  pub fn parse_expr_object(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let loc_start = self.require(TokenType::BraceOpen)?.loc;
    let mut members = Vec::<Node>::new();
    loop {
      if self.peek()?.typ == TokenType::BraceClose {
        break;
      };
      let rest = self.consume_if(TokenType::DotDotDot)?.is_match();
      if rest {
        let value = self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::BraceClose)?;
        let loc = value.loc;
        members.push(Node::new(loc, Syntax::ObjectMember {
          typ: ObjectMemberType::Rest { value },
        }));
      } else {
        let loc_checkpoint = self.checkpoint();
        let ParseClassOrObjectMemberResult {
          key,
          key_loc,
          value,
          ..
        } = self.parse_class_or_object_member(
          ctx,
          TokenType::Colon,
          TokenType::Comma,
          &mut Asi::no(),
        )?;
        members.push(Node::new(
          self.since_checkpoint(loc_checkpoint),
          Syntax::ObjectMember {
            typ: match value {
              ClassOrObjectMemberValue::Property { initializer: None } => {
                ObjectMemberType::Shorthand {
                  identifier: match key {
                    ClassOrObjectMemberKey::Direct(key) => {
                      Node::new(key_loc, Syntax::IdentifierExpr { name: key })
                    }
                    _ => unreachable!(),
                  },
                }
              }
              _ => ObjectMemberType::Valued { key, value },
            },
          },
        ));
      }
      if self.peek()?.typ == TokenType::BraceClose {
        break;
      };
      self.require(TokenType::Comma)?;
    }
    let loc_end = self.require(TokenType::BraceClose)?.loc;
    Ok(Node::new(loc_start + loc_end, Syntax::LiteralObjectExpr {
      members,
    }))
  }

  pub fn parse_expr_arrow_function(
    &mut self,
    ctx: ParseCtx,
    terminator_a: TokenType,
    terminator_b: TokenType,
  ) -> SyntaxResult<Node> {
    let is_async = self.consume_if(TokenType::KeywordAsync)?.is_match();

    let (parameters, arrow) = if !is_async
      && is_valid_pattern_identifier(self.peek()?.typ, ParsePatternRules {
        await_allowed: false,
        yield_allowed: ctx.rules.yield_allowed,
      }) {
      // Single-unparenthesised-parameter arrow function.
      // Parse arrow first for fast fail (and in case we are merely trying to parse as arrow function), before we mutate state by creating nodes and adding symbols.
      let param_name = self.next()?.loc;
      let arrow = self.require(TokenType::EqualsChevronRight)?;
      let pattern = Node::new(param_name, Syntax::IdentifierPattern {
        name: self.string(param_name),
      });
      let param = Node::new(param_name, Syntax::ParamDecl {
        rest: false,
        pattern,
        default_value: None,
      });
      (vec![param], arrow)
    } else {
      let params = self.parse_function_parameters(ctx)?;
      let arrow = self.require(TokenType::EqualsChevronRight)?;
      (params, arrow)
    };

    if arrow.preceded_by_line_terminator {
      // Illegal under Automatic Semicolon Insertion rules.
      return Err(arrow.error(SyntaxErrorType::LineTerminatorAfterArrowFunctionParameters));
    }
    let fn_body_ctx = ctx.with_rules(ParsePatternRules {
      await_allowed: !is_async && ctx.rules.await_allowed,
      ..ctx.rules
    });
    let body = match self.peek()?.typ {
      TokenType::BraceOpen => self.parse_function_body(fn_body_ctx)?,
      _ => self.parse_expr_until_either_with_asi(
        fn_body_ctx,
        terminator_a,
        terminator_b,
        &mut Asi::can(),
      )?,
    };
    Ok(Node::new(body.loc, Syntax::ArrowFunctionExpr {
      parenthesised: false,
      function: Node::new(body.loc, Syntax::Function {
        async_: is_async,
        generator: false,
        parameters,
        body,
      }),
    }))
  }

  pub fn parse_expr_arrow_function_or_grouping(
    &mut self,
    ctx: ParseCtx,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<Node> {
    // Try and parse as arrow function signature first.
    // If we fail, backtrack and parse as grouping instead.
    // After we see `=>`, we assume it's definitely an arrow function and do not backtrack.

    // NOTE: We originally implemented conversion from parameters to expression to prevent the need
    // for backtracking. However, this ended up being too complex for little performance gain,
    // as most usages of grouping involve a non-comma binary operator (such as `+`) and so parsing
    // as arrow function fails quickly. Complex patterns like `{a, b: { c: [d, e] } = f }` are
    // unlikely to be used as operands in a grouping.

    let cp = self.checkpoint();

    match self.parse_expr_arrow_function(ctx, terminator_a, terminator_b) {
      Ok(expr) => Ok(expr),
      Err(err) if err.typ == SyntaxErrorType::LineTerminatorAfterArrowFunctionParameters => {
        Err(err)
      }
      Err(_) => {
        self.restore_checkpoint(cp);
        self.parse_grouping(ctx, asi)
      }
    }
  }

  pub fn parse_expr_import(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordImport)?;
    if self.consume_if(TokenType::Dot)?.is_match() {
      // import.meta
      let prop = self.require(TokenType::Identifier)?;
      if self.str(prop.loc) != "meta" {
        return Err(prop.error(SyntaxErrorType::ExpectedSyntax("`meta` property")));
      };
      return Ok(Node::new(start.loc + prop.loc, Syntax::ImportMeta {}));
    }
    self.require(TokenType::ParenthesisOpen)?;
    let module = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
    let end = self.require(TokenType::ParenthesisClose)?;
    Ok(Node::new(start.loc + end.loc, Syntax::ImportExpr {
      module,
    }))
  }

  pub fn parse_expr_function(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let is_async = self.consume_if(TokenType::KeywordAsync)?.is_match();
    let start = self.require(TokenType::KeywordFunction)?.loc;
    let generator = self.consume_if(TokenType::Asterisk)?.is_match();
    let name = match self.peek()? {
      t if is_valid_pattern_identifier(t.typ, ctx.rules) => {
        self.consume_peeked();
        Some(Node::new(t.loc, Syntax::ClassOrFunctionName {
          name: self.string(t.loc),
        }))
      }
      _ => None,
    };
    let parameters = self.parse_function_parameters(ctx)?;
    let fn_body_ctx = ctx.with_rules(ParsePatternRules {
      await_allowed: !is_async && ctx.rules.await_allowed,
      yield_allowed: !generator && ctx.rules.yield_allowed,
    });
    let body = self.parse_function_body(fn_body_ctx)?;
    Ok(Node::new(start + body.loc, Syntax::FunctionExpr {
      parenthesised: false,
      name,
      function: Node::new(start + body.loc, Syntax::Function {
        async_: is_async,
        generator,
        parameters,
        body,
      }),
    }))
  }

  pub fn parse_expr_class(&mut self, ctx: ParseCtx) -> SyntaxResult<Node> {
    let start = self.require(TokenType::KeywordClass)?.loc;
    let name = match self.peek()? {
      t if is_valid_pattern_identifier(t.typ, ctx.rules) => {
        self.consume_peeked();
        let name_node = Node::new(t.loc, Syntax::ClassOrFunctionName {
          name: self.string(t.loc),
        });
        Some(name_node)
      }
      _ => None,
    };
    let extends = if self.consume_if(TokenType::KeywordExtends)?.is_match() {
      Some(self.parse_expr(ctx, TokenType::BraceOpen)?)
    } else {
      None
    };
    let ParseClassBodyResult { end, members } = self.parse_class_body(ctx)?;
    Ok(Node::new(start + end, Syntax::ClassExpr {
      parenthesised: false,
      name,
      extends,
      members,
    }))
  }

  // NOTE: The next token must definitely be LiteralTemplatePartString{,End}.
  fn parse_expr_literal_template_parts(
    &mut self,
    ctx: ParseCtx,
  ) -> SyntaxResult<Vec<LiteralTemplatePart>> {
    let t = self.next().unwrap();
    let is_end = match t.typ {
      TokenType::LiteralTemplatePartString => false,
      TokenType::LiteralTemplatePartStringEnd => true,
      _ => unreachable!(),
    };

    let mut loc = t.loc;
    let mut parts = Vec::new();
    parts.push(LiteralTemplatePart::String(
      normalise_literal_string_or_template_inner(self.bytes(t.loc))
        .ok_or_else(|| t.loc.error(SyntaxErrorType::InvalidCharacterEscape, None))?,
    ));
    if !is_end {
      loop {
        let substitution = self.parse_expr(ctx, TokenType::BraceClose)?;
        self.require(TokenType::BraceClose)?;
        parts.push(LiteralTemplatePart::Substitution(substitution));
        let string = lex_template_string_continue(self.lexer_mut(), false)?;
        loc.extend(string.loc);
        parts.push(LiteralTemplatePart::String(
          normalise_literal_string_or_template_inner(self.bytes(string.loc)).ok_or_else(|| {
            string
              .loc
              .error(SyntaxErrorType::InvalidCharacterEscape, None)
          })?,
        ));
        self.clear_buffered();
        match string.typ {
          TokenType::LiteralTemplatePartStringEnd => break,
          _ => {}
        };
      }
    };

    Ok(parts)
  }

  fn parse_expr_operand(
    &mut self,
    ctx: ParseCtx,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<Node> {
    let cp = self.checkpoint();
    let t = self.next_with_mode(LexMode::SlashIsRegex)?;
    let operand = match UNARY_OPERATOR_MAPPING.get(&t.typ) {
      Some(operator)
        if (
          // TODO Is this correct? Should it be possible to use as operator or keyword depending on whether there is an operand following?
          (operator.name != OperatorName::Await && operator.name != OperatorName::Yield)
            || (operator.name == OperatorName::Await && !ctx.rules.await_allowed)
            || (operator.name == OperatorName::Yield && !ctx.rules.yield_allowed)
        ) =>
      {
        let operator = if operator.name == OperatorName::Yield
          && self.consume_if(TokenType::Asterisk)?.is_match()
        {
          &OPERATORS[&OperatorName::YieldDelegated]
        } else {
          *operator
        };
        let next_min_prec =
          operator.precedence + (operator.associativity == Associativity::Left) as u8;
        let operand = self.parse_expr_with_min_prec(
          ctx,
          next_min_prec,
          terminator_a,
          terminator_b,
          false,
          asi,
        )?;
        Node::new(t.loc + operand.loc, Syntax::UnaryExpr {
          parenthesised: false,
          operator: operator.name,
          argument: operand,
        })
      }
      _ => {
        match t.typ {
          TokenType::BracketOpen => {
            self.restore_checkpoint(cp);
            self.parse_expr_array(ctx)?
          }
          TokenType::BraceOpen => {
            self.restore_checkpoint(cp);
            self.parse_expr_object(ctx)?
          }
          TokenType::ChevronLeft => {
            self.restore_checkpoint(cp);
            self.parse_jsx_element(ctx)?
          }
          // Check this before is_valid_pattern_identifier.
          TokenType::KeywordAsync => {
            match self.peek()?.typ {
              TokenType::ParenthesisOpen => {
                self.restore_checkpoint(cp);
                self.parse_expr_arrow_function(ctx, terminator_a, terminator_b)?
              }
              TokenType::KeywordFunction => {
                self.restore_checkpoint(cp);
                self.parse_expr_function(ctx)?
              }
              _ => {
                // `await` is being used as an identifier.
                Node::new(t.loc, Syntax::IdentifierExpr {
                  name: self.string(t.loc),
                })
              }
            }
          }
          typ if is_valid_pattern_identifier(typ, ctx.rules) => {
            if self.peek()?.typ == TokenType::EqualsChevronRight {
              // Single-unparenthesised-parameter arrow function.
              // NOTE: `await` is not allowed as an arrow function parameter, but we'll check this in parse_expr_arrow_function.
              self.restore_checkpoint(cp);
              self.parse_expr_arrow_function(ctx, terminator_a, terminator_b)?
            } else {
              Node::new(t.loc, Syntax::IdentifierExpr {
                name: self.string(t.loc),
              })
            }
          }
          TokenType::KeywordClass => {
            self.restore_checkpoint(cp);
            self.parse_expr_class(ctx)?
          }
          TokenType::KeywordFunction => {
            self.restore_checkpoint(cp);
            self.parse_expr_function(ctx)?
          }
          TokenType::KeywordImport => {
            self.restore_checkpoint(cp);
            self.parse_expr_import(ctx)?
          }
          TokenType::KeywordSuper => Node::new(t.loc, Syntax::SuperExpr {}),
          TokenType::KeywordThis => Node::new(t.loc, Syntax::ThisExpr {}),
          TokenType::LiteralBigInt => Node::new(t.loc, Syntax::LiteralBigIntExpr {
            value: normalise_literal_bigint(self.str(t.loc))
              .ok_or_else(|| t.loc.error(SyntaxErrorType::MalformedLiteralBigInt, None))?,
          }),
          TokenType::LiteralTrue | TokenType::LiteralFalse => {
            Node::new(t.loc, Syntax::LiteralBooleanExpr {
              value: t.typ == TokenType::LiteralTrue,
            })
          }
          TokenType::LiteralNull => Node::new(t.loc, Syntax::LiteralNull {}),
          TokenType::LiteralNumber => Node::new(t.loc, Syntax::LiteralNumberExpr {
            value: normalise_literal_number(self.str(t.loc))
              .ok_or_else(|| t.loc.error(SyntaxErrorType::MalformedLiteralNumber, None))?,
          }),
          TokenType::LiteralRegex => Node::new(t.loc, Syntax::LiteralRegexExpr {
            value: self.string(t.loc),
          }),
          TokenType::LiteralString => Node::new(t.loc, Syntax::LiteralStringExpr {
            value: normalise_literal_string(self.str(t.loc))
              .ok_or_else(|| t.loc.error(SyntaxErrorType::InvalidCharacterEscape, None))?,
          }),
          TokenType::LiteralTemplatePartString | TokenType::LiteralTemplatePartStringEnd => {
            self.restore_checkpoint(cp);
            let parts = self.parse_expr_literal_template_parts(ctx)?;
            Node::new(t.loc, Syntax::LiteralTemplateExpr { parts })
          }
          TokenType::ParenthesisOpen => {
            self.restore_checkpoint(cp);
            self.parse_expr_arrow_function_or_grouping(ctx, terminator_a, terminator_b, asi)?
          }
          _ => return Err(t.error(SyntaxErrorType::ExpectedSyntax("expression operand"))),
        }
      }
    };
    Ok(operand)
  }

  pub fn parse_expr_with_min_prec(
    &mut self,
    ctx: ParseCtx,
    min_prec: u8,
    terminator_a: TokenType,
    terminator_b: TokenType,
    parenthesised: bool,
    asi: &mut Asi,
  ) -> SyntaxResult<Node> {
    let mut left = self.parse_expr_operand(ctx, terminator_a, terminator_b, asi)?;

    loop {
      let cp = self.checkpoint();
      let t = self.next()?;

      if t.typ == terminator_a || t.typ == terminator_b {
        self.restore_checkpoint(cp);
        break;
      };

      match t.typ {
        // Automatic Semicolon Insertion rules: no newline between operand and postfix operator.
        TokenType::PlusPlus | TokenType::HyphenHyphen if !t.preceded_by_line_terminator => {
          let operator_name = match t.typ {
            TokenType::PlusPlus => OperatorName::PostfixIncrement,
            TokenType::HyphenHyphen => OperatorName::PostfixDecrement,
            _ => unreachable!(),
          };
          let operator = &OPERATORS[&operator_name];
          if operator.precedence < min_prec {
            self.restore_checkpoint(cp);
            break;
          };
          left = Node::new(left.loc + t.loc, Syntax::UnaryPostfixExpr {
            parenthesised: false,
            operator: operator_name,
            argument: left,
          });
          continue;
        }
        // Automatic Semicolon Insertion rules: no newline between operand and template literal.
        TokenType::LiteralTemplatePartString | TokenType::LiteralTemplatePartStringEnd
          if !t.preceded_by_line_terminator =>
        {
          let loc = t.loc;
          self.restore_checkpoint(cp);
          let parts = self.parse_expr_literal_template_parts(ctx)?;
          left = Node::new(left.loc + loc, Syntax::TaggedTemplateExpr {
            function: left,
            parts,
          });
          continue;
        }
        _ => {}
      };

      match MULTARY_OPERATOR_MAPPING.get(&t.typ) {
        None => {
          if asi.can_end_with_asi
            && (t.preceded_by_line_terminator
              || t.typ == TokenType::BraceClose
              || t.typ == TokenType::EOF)
          {
            // Automatic Semicolon Insertion.
            // TODO Exceptions (e.g. for loop header).
            self.restore_checkpoint(cp);
            asi.did_end_with_asi = true;
            break;
          };
          return Err(t.error(SyntaxErrorType::ExpectedSyntax("expression operator")));
        }
        Some(operator) => {
          if operator.precedence < min_prec {
            self.restore_checkpoint(cp);
            break;
          };

          let next_min_prec =
            operator.precedence + (operator.associativity == Associativity::Left) as u8;

          left = match operator.name {
            OperatorName::Call | OperatorName::OptionalChainingCall => {
              let arguments = self.parse_call_args(ctx)?;
              let end = self.require(TokenType::ParenthesisClose)?;
              Node::new(left.loc + end.loc, Syntax::CallExpr {
                parenthesised: false,
                optional_chaining: match operator.name {
                  OperatorName::OptionalChainingCall => true,
                  _ => false,
                },
                arguments,
                callee: left,
              })
            }
            OperatorName::ComputedMemberAccess
            | OperatorName::OptionalChainingComputedMemberAccess => {
              let member = self.parse_expr(ctx, TokenType::BracketClose)?;
              let end = self.require(TokenType::BracketClose)?;
              Node::new(left.loc + end.loc, Syntax::ComputedMemberExpr {
                optional_chaining: match operator.name {
                  OperatorName::OptionalChainingComputedMemberAccess => true,
                  _ => false,
                },
                object: left,
                member,
              })
            }
            OperatorName::Conditional => {
              let consequent = self.parse_expr(ctx, TokenType::Colon)?;
              self.require(TokenType::Colon)?;
              let alternate = self.parse_expr_with_min_prec(
                ctx,
                OPERATORS[&OperatorName::ConditionalAlternate].precedence,
                terminator_a,
                terminator_b,
                false,
                asi,
              )?;
              Node::new(left.loc + alternate.loc, Syntax::ConditionalExpr {
                parenthesised: false,
                test: left,
                consequent,
                alternate,
              })
            }
            OperatorName::MemberAccess | OperatorName::OptionalChainingMemberAccess => {
              let right_tok = self.next()?;
              match right_tok.typ {
                TokenType::Identifier => {}
                TokenType::PrivateMember => {}
                t if KEYWORDS_MAPPING.contains_key(&t) => {}
                _ => {
                  return Err(
                    right_tok.error(SyntaxErrorType::ExpectedSyntax("member access property")),
                  )
                }
              };
              let right = right_tok.loc;
              Node::new(left.loc + right, Syntax::MemberExpr {
                parenthesised: false,
                optional_chaining: match operator.name {
                  OperatorName::OptionalChainingMemberAccess => true,
                  _ => false,
                },
                left,
                right: self.string(right),
              })
            }
            _ => {
              if operator.name.is_assignment() {
                left = self.convert_assignment_lhs_to_target(ctx, left, operator.name)?;
              };
              let right = self.parse_expr_with_min_prec(
                ctx,
                next_min_prec,
                terminator_a,
                terminator_b,
                false,
                asi,
              )?;
              Node::new(left.loc + right.loc, Syntax::BinaryExpr {
                parenthesised: false,
                operator: operator.name,
                left,
                right,
              })
            }
          };
        }
      };
    }

    if parenthesised {
      match left.stx.as_mut() {
        Syntax::ArrowFunctionExpr { parenthesised, .. }
        | Syntax::BinaryExpr { parenthesised, .. }
        | Syntax::CallExpr { parenthesised, .. }
        | Syntax::ConditionalExpr { parenthesised, .. }
        | Syntax::FunctionExpr { parenthesised, .. }
        | Syntax::MemberExpr { parenthesised, .. }
        | Syntax::UnaryExpr { parenthesised, .. } => {
          *parenthesised = true;
        }
        _ => {}
      };
    };

    Ok(left)
  }
}
