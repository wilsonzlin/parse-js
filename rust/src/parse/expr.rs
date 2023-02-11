use super::class_or_object::ParseClassBodyResult;
use super::class_or_object::ParseClassOrObjectMemberResult;
use super::literal::normalise_literal_bigint;
use super::pattern::is_valid_pattern_identifier;
use super::pattern::ParsePatternSyntax;
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
use crate::session::SessionVec;
use crate::symbol::ScopeFlag;
use crate::symbol::ScopeType;
use crate::token::TokenType;
use std::cell::Ref;

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

// Trying to check if every object, array, or identifier expression operand is actually an assignment target first is too expensive and wasteful, so simply retroactively transform the LHS of a BinaryExpr with Assignment* operator into a target, raising an error if it can't (and is an invalid assignment target). A valid target is:
// - A chain of non-optional-chaining member, computed member, and call operators, not ending in a call.
// - A pattern.
fn convert_assignment_lhs_to_target<'a>(
  ctx: ParseCtx<'a>,
  lhs: Node<'a>,
  operator_name: OperatorName,
) -> SyntaxResult<'a, Node<'a>> {
  match &*lhs.stx() {
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
      let root = transform_literal_expr_to_destructuring_pattern(ctx, lhs)?;
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

fn is_chevron_right_or_slash(typ: TokenType) -> bool {
  typ == TokenType::ChevronRight || typ == TokenType::Slash
}

fn jsx_tag_names_are_equal(a: Option<Ref<'_, Syntax>>, b: Option<Ref<'_, Syntax>>) -> bool {
  match (a.as_deref(), b.as_deref()) {
    (None, None) => true,
    (
      Some(Syntax::JsxMember {
        base: a_base,
        path: a_path,
      }),
      Some(Syntax::JsxMember {
        base: b_base,
        path: b_path,
      }),
    ) => a_base == b_base && a_path == b_path,
    (
      Some(Syntax::JsxName {
        name: a_name,
        namespace: a_ns,
      }),
      Some(Syntax::JsxName {
        name: b_name,
        namespace: b_ns,
      }),
    ) => a_ns == b_ns && a_name == b_name,
    _ => false,
  }
}

fn transform_literal_expr_to_destructuring_pattern<'a>(
  ctx: ParseCtx<'a>,
  node: Node<'a>,
) -> SyntaxResult<'a, Node<'a>> {
  let loc = node.loc();
  match &*node.stx() {
    Syntax::LiteralArrayExpr { elements } => {
      let mut pat_elements = ctx.session.new_vec::<Option<ArrayPatternElement>>();
      let mut rest = None;
      for element in elements {
        if rest.is_some() {
          return Err(node.error(SyntaxErrorType::InvalidAssigmentTarget));
        };
        match element {
          ArrayElement::Single(elem) => {
            match *elem.stx() {
              Syntax::BinaryExpr {
                parenthesised,
                operator,
                left,
                right,
              } => {
                if parenthesised || operator != OperatorName::Assignment {
                  return Err(node.error(SyntaxErrorType::InvalidAssigmentTarget));
                };
                pat_elements.push(Some(ArrayPatternElement {
                  target: transform_literal_expr_to_destructuring_pattern(ctx, left)?,
                  default_value: Some(right),
                }));
              }
              _ => pat_elements.push(Some(ArrayPatternElement {
                target: transform_literal_expr_to_destructuring_pattern(ctx, *elem)?,
                default_value: None,
              })),
            };
          }
          ArrayElement::Rest(expr) => {
            rest = Some(transform_literal_expr_to_destructuring_pattern(ctx, *expr)?);
          }
          ArrayElement::Empty => pat_elements.push(None),
        };
      }
      Ok(ctx.create_node(loc.clone(), Syntax::ArrayPattern {
        elements: pat_elements,
        rest,
      }))
    }
    Syntax::LiteralObjectExpr { members } => {
      let mut properties = ctx.session.new_vec();
      let mut rest = None;
      for member in members {
        if rest.is_some() {
          return Err(node.error(SyntaxErrorType::InvalidAssigmentTarget));
        };
        match &mut *member.stx_mut() {
          Syntax::ObjectMember { typ } => match typ {
            ObjectMemberType::Valued { key, value } => {
              let (target, default_value) = match value {
                ClassOrObjectMemberValue::Property {
                  initializer: Some(initializer),
                } => match *initializer.stx() {
                  Syntax::BinaryExpr {
                    parenthesised,
                    operator,
                    left,
                    right,
                  } => {
                    if parenthesised || operator != OperatorName::Assignment {
                      return Err(node.error(SyntaxErrorType::InvalidAssigmentTarget));
                    };
                    (
                      transform_literal_expr_to_destructuring_pattern(ctx, left)?,
                      Some(right),
                    )
                  }
                  _ => (
                    transform_literal_expr_to_destructuring_pattern(ctx, *initializer)?,
                    None,
                  ),
                },
                _ => return Err(node.error(SyntaxErrorType::InvalidAssigmentTarget)),
              };
              properties.push(ctx.create_node(loc.clone(), Syntax::ObjectPatternProperty {
                key: *key,
                target: Some(target),
                default_value,
              }));
            }
            ObjectMemberType::Shorthand { name } => {
              properties.push(ctx.create_node(loc.clone(), Syntax::ObjectPatternProperty {
                key: ClassOrObjectMemberKey::Direct(name.clone()),
                target: None,
                default_value: None,
              }));
            }
            ObjectMemberType::Rest { value } => {
              rest = Some(transform_literal_expr_to_destructuring_pattern(
                ctx, *value,
              )?);
            }
          },
          _ => unreachable!(),
        };
      }
      Ok(ctx.create_node(loc.clone(), Syntax::ObjectPattern { properties, rest }))
    }
    // It's possible to encounter an IdentifierPattern e.g. `{ a: b = 1 } = x`, where `b = 1` is already parsed as an assignment.
    Syntax::IdentifierExpr { name } | Syntax::IdentifierPattern { name } => {
      Ok(ctx.create_node(loc.clone(), Syntax::IdentifierPattern {
        name: name.clone(),
      }))
    }
    _ => Err(node.error(SyntaxErrorType::InvalidAssigmentTarget)),
  }
}

impl<'a> Parser<'a> {
  pub fn parse_jsx_name(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let start = self.require_with_mode(TokenType::Identifier, LexMode::JsxTag)?;
    Ok(if self.consume_if(TokenType::Colon)?.is_match() {
      // Namespaced name.
      let name = self.require_with_mode(TokenType::Identifier, LexMode::JsxTag)?;
      ctx.create_node(start.loc() + name.loc(), Syntax::JsxName {
        namespace: Some(start.loc().clone()),
        name: name.loc().clone(),
      })
    } else {
      // Plain name.
      ctx.create_node(start.loc().clone(), Syntax::JsxName {
        namespace: None,
        name: start.loc().clone(),
      })
    })
  }

  pub fn parse_jsx_tag_name(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Option<Node<'a>>> {
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
            ctx.create_node(start + name.loc(), Syntax::JsxName {
              namespace: Some(start.clone()),
              name: name.loc().clone(),
            })
          } else if self.peek()?.typ() == TokenType::Dot && !start.as_slice().contains(&b'-') {
            // Member name.
            let mut path = ctx.session.new_vec();
            while self.consume_if(TokenType::Dot)?.is_match() {
              path.push(self.require(TokenType::Identifier)?.loc().clone());
            }
            ctx.create_node(start.add_option(path.last().copied()), Syntax::JsxMember {
              base: start.clone(),
              path,
            })
          } else {
            // Plain name.
            ctx.create_node(start.clone(), Syntax::JsxName {
              namespace: None,
              name: start.clone(),
            })
          }
        }),
      },
    )
  }

  // https://facebook.github.io/jsx/
  pub fn parse_jsx_element(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let tag_start = self.require(TokenType::ChevronLeft)?;
    let tag_name = self.parse_jsx_tag_name(ctx)?;

    // Attributes.
    let mut attributes = ctx.session.new_vec();
    if tag_name.is_some() {
      loop {
        if is_chevron_right_or_slash(self.peek()?.typ()) {
          break;
        }
        if self.consume_if(TokenType::BraceOpen)?.is_match() {
          let start = self.require(TokenType::DotDotDot)?;
          let value = self.parse_expr(ctx, TokenType::BraceClose)?;
          let end = self.require(TokenType::BraceClose)?;
          attributes.push(
            ctx.create_node(start.loc() + end.loc(), Syntax::JsxSpreadAttribute {
              value,
            }),
          );
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
            let expr = ctx.create_node(value.loc().clone(), Syntax::JsxExpressionContainer {
              value,
            });
            self.require(TokenType::BraceClose)?;
            expr
          } else {
            let value = self.require(TokenType::LiteralString)?;
            ctx.create_node(value.loc().clone(), Syntax::JsxText {
              value: value.loc().clone(),
            })
          })
        };
        attributes.push(ctx.create_node(
          name.loc().add_option(value.map(|n| n.loc())),
          Syntax::JsxAttribute { name, value },
        ))
      }
    }

    Ok(if self.consume_if(TokenType::Slash)?.is_match() {
      // Self closing.
      let end = self.require(TokenType::ChevronRight)?;
      ctx.create_node(tag_start.loc() + end.loc(), Syntax::JsxElement {
        name: tag_name,
        attributes,
        children: ctx.session.new_vec(),
      })
    } else {
      self.require(TokenType::ChevronRight)?;

      // Children.
      let mut children = ctx.session.new_vec();
      let close_start = loop {
        match self.consume_if(TokenType::ChevronLeftSlash)? {
          t if t.is_match() => break t,
          _ => {}
        };
        let text = self.require_with_mode(TokenType::JsxTextContent, LexMode::JsxTextContent)?;
        if !text.loc().is_empty() {
          children.push(ctx.create_node(text.loc().clone(), Syntax::JsxText {
            value: text.loc().clone(),
          }));
        };
        if self.peek()?.typ() == TokenType::ChevronLeft {
          children.push(self.parse_jsx_element(ctx)?);
        };
        if self.consume_if(TokenType::BraceOpen)?.is_match() {
          // TODO Allow empty expr.
          let value = self.parse_expr(ctx, TokenType::BraceClose)?;
          children.push(
            ctx.create_node(value.loc().clone(), Syntax::JsxExpressionContainer {
              value,
            }),
          );
          self.require(TokenType::BraceClose)?;
        };
      };
      let end_name = self.parse_jsx_tag_name(ctx)?;
      if !jsx_tag_names_are_equal(tag_name.map(|n| n.stx()), end_name.map(|n| n.stx())) {
        return Err(close_start.error(SyntaxErrorType::JsxClosingTagMismatch));
      };
      let end = self.require(TokenType::ChevronRight)?;
      ctx.create_node(tag_start.loc() + end.loc(), Syntax::JsxElement {
        name: tag_name,
        attributes,
        children,
      })
    })
  }

  pub fn parse_call_args(
    &mut self,
    ctx: ParseCtx<'a>,
  ) -> SyntaxResult<'a, SessionVec<'a, Node<'a>>> {
    let mut args = ctx.session.new_vec();
    loop {
      if self.peek()?.typ() == TokenType::ParenthesisClose {
        break;
      };
      let spread = self.consume_if(TokenType::DotDotDot)?.is_match();
      let value =
        self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::ParenthesisClose)?;
      args.push(ctx.create_node(value.loc().clone(), Syntax::CallArg { spread, value }));
      if !self.consume_if(TokenType::Comma)?.is_match() {
        break;
      };
    }
    Ok(args)
  }

  pub fn parse_expr(
    &mut self,
    ctx: ParseCtx<'a>,
    terminator: TokenType,
  ) -> SyntaxResult<'a, Node<'a>> {
    self.parse_expr_with_min_prec(ctx, 1, terminator, TokenType::_Dummy, false, &mut Asi::no())
  }

  pub fn parse_expr_with_asi(
    &mut self,
    ctx: ParseCtx<'a>,
    terminator: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<'a, Node<'a>> {
    self.parse_expr_with_min_prec(ctx, 1, terminator, TokenType::_Dummy, false, asi)
  }

  pub fn parse_expr_until_either(
    &mut self,
    ctx: ParseCtx<'a>,
    terminator_a: TokenType,
    terminator_b: TokenType,
  ) -> SyntaxResult<'a, Node<'a>> {
    self.parse_expr_with_min_prec(ctx, 1, terminator_a, terminator_b, false, &mut Asi::no())
  }

  pub fn parse_expr_until_either_with_asi(
    &mut self,
    ctx: ParseCtx<'a>,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<'a, Node<'a>> {
    self.parse_expr_with_min_prec(ctx, 1, terminator_a, terminator_b, false, asi)
  }

  pub fn parse_grouping(&mut self, ctx: ParseCtx<'a>, asi: &mut Asi) -> SyntaxResult<'a, Node<'a>> {
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

  pub fn parse_expr_array(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let loc_start = self.require(TokenType::BracketOpen)?.loc_take();
    let mut elements = ctx.session.new_vec::<ArrayElement>();
    loop {
      if self.consume_if(TokenType::Comma)?.is_match() {
        elements.push(ArrayElement::Empty);
        continue;
      };
      if self.peek()?.typ() == TokenType::BracketClose {
        break;
      };
      let rest = self.consume_if(TokenType::DotDotDot)?.is_match();
      let value = self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::BracketClose)?;
      elements.push(if rest {
        ArrayElement::Rest(value)
      } else {
        ArrayElement::Single(value)
      });
      if self.peek()?.typ() == TokenType::BracketClose {
        break;
      };
      self.require(TokenType::Comma)?;
    }
    let loc_end = self.require(TokenType::BracketClose)?.loc_take();
    Ok(ctx.create_node(loc_start + loc_end, Syntax::LiteralArrayExpr { elements }))
  }

  pub fn parse_expr_object(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let loc_start = self.require(TokenType::BraceOpen)?.loc_take();
    let mut members = ctx.session.new_vec::<Node<'a>>();
    loop {
      if self.peek()?.typ() == TokenType::BraceClose {
        break;
      };
      let rest = self.consume_if(TokenType::DotDotDot)?.is_match();
      if rest {
        let value = self.parse_expr_until_either(ctx, TokenType::Comma, TokenType::BraceClose)?;
        let loc = value.loc().clone();
        members.push(ctx.create_node(loc, Syntax::ObjectMember {
          typ: ObjectMemberType::Rest { value },
        }));
      } else {
        let loc_checkpoint = self.checkpoint();
        let ParseClassOrObjectMemberResult { key, value } = self.parse_class_or_object_member(
          ctx,
          TokenType::Colon,
          TokenType::Comma,
          &mut Asi::no(),
        )?;
        members.push(ctx.create_node(
          self.since_checkpoint(loc_checkpoint),
          Syntax::ObjectMember {
            typ: match value {
              ClassOrObjectMemberValue::Property { initializer: None } => {
                ObjectMemberType::Shorthand {
                  name: match key {
                    ClassOrObjectMemberKey::Direct(key) => key.clone(),
                    _ => unreachable!(),
                  },
                }
              }
              _ => ObjectMemberType::Valued { key, value },
            },
          },
        ));
      }
      if self.peek()?.typ() == TokenType::BraceClose {
        break;
      };
      self.require(TokenType::Comma)?;
    }
    let loc_end = self.require(TokenType::BraceClose)?.loc_take();
    Ok(ctx.create_node(loc_start + loc_end, Syntax::LiteralObjectExpr { members }))
  }

  pub fn parse_expr_arrow_function(
    &mut self,
    ctx: ParseCtx<'a>,
    terminator_a: TokenType,
    terminator_b: TokenType,
  ) -> SyntaxResult<'a, Node<'a>> {
    let fn_scope = ctx.create_child_scope(ScopeType::ArrowFunction);
    let fn_ctx = ctx.with_scope(fn_scope);

    let is_async = self.consume_if(TokenType::KeywordAsync)?.is_match();

    let (signature, arrow) = if !is_async
      && is_valid_pattern_identifier(self.peek()?.typ(), ParsePatternSyntax {
        await_allowed: false,
        yield_allowed: ctx.syntax.yield_allowed,
      }) {
      // Single-unparenthesised-parameter arrow function.
      // Parse arrow first for fast fail (and in case we are merely trying to parse as arrow function), before we mutate state by creating nodes and adding symbols.
      let param_name = self.next()?.loc_take();
      let arrow = self.require(TokenType::EqualsChevronRight)?;
      let pattern = fn_ctx.create_node(param_name.clone(), Syntax::IdentifierPattern {
        name: param_name.clone(),
      });
      fn_scope.add_block_symbol(param_name.clone(), pattern)?;
      let param = ctx.create_node(param_name.clone(), Syntax::ParamDecl {
        rest: false,
        pattern,
        default_value: None,
      });
      let signature = ctx.create_node(param_name.clone(), Syntax::FunctionSignature {
        parameters: {
          let mut params = ctx.session.new_vec();
          params.push(param);
          params
        },
      });
      (signature, arrow)
    } else {
      let signature = self.parse_signature_function(fn_ctx)?;
      let arrow = self.require(TokenType::EqualsChevronRight)?;
      (signature, arrow)
    };

    if arrow.preceded_by_line_terminator() {
      // Illegal under Automatic Semicolon Insertion rules.
      return Err(arrow.error(SyntaxErrorType::LineTerminatorAfterArrowFunctionParameters));
    }
    let fn_body_ctx = fn_ctx.with_syntax(ParsePatternSyntax {
      await_allowed: !is_async && ctx.syntax.await_allowed,
      ..ctx.syntax
    });
    let body = match self.peek()?.typ() {
      TokenType::BraceOpen => self.parse_stmt(fn_body_ctx)?,
      _ => self.parse_expr_until_either_with_asi(
        fn_body_ctx,
        terminator_a,
        terminator_b,
        &mut Asi::can(),
      )?,
    };
    Ok(
      ctx.create_node(signature.loc() + body.loc(), Syntax::ArrowFunctionExpr {
        parenthesised: false,
        is_async,
        signature,
        body,
      }),
    )
  }

  pub fn parse_expr_arrow_function_or_grouping(
    &mut self,
    ctx: ParseCtx<'a>,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<'a, Node<'a>> {
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
      Err(err) if err.typ() == SyntaxErrorType::LineTerminatorAfterArrowFunctionParameters => {
        Err(err)
      }
      Err(_) => {
        self.restore_checkpoint(cp);
        self.parse_grouping(ctx, asi)
      }
    }
  }

  pub fn parse_expr_import(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let start = self.require(TokenType::KeywordImport)?;
    if self.consume_if(TokenType::Dot)?.is_match() {
      // import.meta
      let prop = self.require(TokenType::Identifier)?;
      if prop.loc() != "meta" {
        return Err(prop.error(SyntaxErrorType::ExpectedSyntax("`meta` property")));
      };
      return Ok(ctx.create_node(start.loc() + prop.loc(), Syntax::ImportMeta {}));
    }
    self.require(TokenType::ParenthesisOpen)?;
    let module = self.parse_expr(ctx, TokenType::ParenthesisClose)?;
    self.require(TokenType::ParenthesisClose)?;
    let end = self.require(TokenType::ParenthesisClose)?;
    Ok(ctx.create_node(start.loc() + end.loc(), Syntax::ImportExpr { module }))
  }

  pub fn parse_expr_function(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let fn_scope = ctx.create_child_scope(ScopeType::NonArrowFunction);
    let fn_ctx = ctx.with_scope(fn_scope);

    let is_async = self.consume_if(TokenType::KeywordAsync)?.is_match();
    let start = self.require(TokenType::KeywordFunction)?.loc().clone();
    let generator = self.consume_if(TokenType::Asterisk)?.is_match();
    // WARNING: Unlike function declarations, function expressions are not declared within their current closure or block. However, their names cannot be assigned to within the function (it has no effect) and they can be "redeclared" e.g. `(function a() { let a = 1; })()`.
    let name = match self.peek()? {
      t if is_valid_pattern_identifier(t.typ(), ctx.syntax) => {
        self.consume_peeked();
        let name_node = fn_ctx.create_node(t.loc().clone(), Syntax::ClassOrFunctionName {
          name: t.loc().clone(),
        });
        fn_scope.add_symbol(t.loc().clone(), name_node)?;
        Some(name_node)
      }
      _ => None,
    };
    let signature = self.parse_signature_function(fn_ctx)?;
    let fn_body_ctx = fn_ctx.with_syntax(ParsePatternSyntax {
      await_allowed: !is_async && ctx.syntax.await_allowed,
      yield_allowed: !generator && ctx.syntax.yield_allowed,
    });
    let body = self.parse_stmt_block(fn_body_ctx)?;
    Ok(ctx.create_node(start + body.loc(), Syntax::FunctionExpr {
      parenthesised: false,
      is_async,
      generator,
      name,
      signature,
      body,
    }))
  }

  pub fn parse_expr_class(&mut self, ctx: ParseCtx<'a>) -> SyntaxResult<'a, Node<'a>> {
    let start = self.require(TokenType::KeywordClass)?.loc().clone();
    let name = match self.peek()? {
      t if is_valid_pattern_identifier(t.typ(), ctx.syntax) => {
        self.consume_peeked();
        let name_node = ctx.create_node(t.loc().clone(), Syntax::ClassOrFunctionName {
          name: t.loc().clone(),
        });
        ctx.scope.add_symbol(t.loc().clone(), name_node)?;
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
    Ok(ctx.create_node(start + end, Syntax::ClassExpr {
      parenthesised: false,
      name,
      extends,
      members,
    }))
  }

  fn parse_expr_operand(
    &mut self,
    ctx: ParseCtx<'a>,
    terminator_a: TokenType,
    terminator_b: TokenType,
    asi: &mut Asi,
  ) -> SyntaxResult<'a, Node<'a>> {
    let cp = self.checkpoint();
    let t = self.next_with_mode(LexMode::SlashIsRegex)?;
    let operand = match UNARY_OPERATOR_MAPPING.get(&t.typ()) {
      Some(operator)
        if (
          // TODO Is this correct? Should it be possible to use as operator or keyword depending on whether there is an operand following?
          (operator.name != OperatorName::Await && operator.name != OperatorName::Yield)
            || (operator.name == OperatorName::Await && !ctx.syntax.await_allowed)
            || (operator.name == OperatorName::Yield && !ctx.syntax.yield_allowed)
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
        ctx.create_node(t.loc() + operand.loc(), Syntax::UnaryExpr {
          parenthesised: false,
          operator: operator.name,
          argument: operand,
        })
      }
      _ => {
        match t.typ() {
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
            match self.peek()?.typ() {
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
                ctx.create_node(t.loc().clone(), Syntax::IdentifierExpr {
                  name: t.loc().clone(),
                })
              }
            }
          }
          typ if is_valid_pattern_identifier(typ, ctx.syntax) => {
            if self.peek()?.typ() == TokenType::EqualsChevronRight {
              // Single-unparenthesised-parameter arrow function.
              // NOTE: `await` is not allowed as an arrow function parameter, but we'll check this in parse_expr_arrow_function.
              self.restore_checkpoint(cp);
              self.parse_expr_arrow_function(ctx, terminator_a, terminator_b)?
            } else {
              if t.loc() == "arguments"
                && ctx
                  .scope
                  .find_symbol_up_to_nearest_scope_of_type(t.loc(), ScopeType::NonArrowFunction)
                  .is_none()
              {
                if let Some(mut closure) = ctx
                  .scope
                  .find_self_or_ancestor(|t| t == ScopeType::ArrowFunction)
                {
                  closure.set_flag(ScopeFlag::UsesArguments);
                };
              };
              ctx.create_node(t.loc().clone(), Syntax::IdentifierExpr {
                name: t.loc().clone(),
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
          TokenType::KeywordSuper => ctx.create_node(t.loc().clone(), Syntax::SuperExpr {}),
          TokenType::KeywordThis => {
            let new_node = ctx.create_node(t.loc().clone(), Syntax::ThisExpr {});
            if let Some(mut closure) = ctx
              .scope
              .find_self_or_ancestor(|t| t == ScopeType::Class || t == ScopeType::NonArrowFunction)
            {
              closure.set_flag(ScopeFlag::UsesThis);
            };
            new_node
          }
          TokenType::LiteralBigInt => ctx.create_node(t.loc().clone(), Syntax::LiteralBigIntExpr {
            value: normalise_literal_bigint(ctx, t.loc())?,
          }),
          TokenType::LiteralTrue | TokenType::LiteralFalse => {
            ctx.create_node(t.loc().clone(), Syntax::LiteralBooleanExpr {
              value: t.typ() == TokenType::LiteralTrue,
            })
          }
          TokenType::LiteralNull => ctx.create_node(t.loc().clone(), Syntax::LiteralNull {}),
          TokenType::LiteralNumber => ctx.create_node(t.loc().clone(), Syntax::LiteralNumberExpr {
            value: normalise_literal_number(t.loc())?,
          }),
          TokenType::LiteralRegex => ctx.create_node(t.loc().clone(), Syntax::LiteralRegexExpr {}),
          TokenType::LiteralString => ctx.create_node(t.loc().clone(), Syntax::LiteralStringExpr {
            value: normalise_literal_string(ctx, t.loc())?,
          }),
          TokenType::LiteralTemplatePartString => {
            let mut loc = t.loc().clone();
            let mut parts = ctx.session.new_vec();
            parts.push(LiteralTemplatePart::String(t.loc().clone()));
            loop {
              let substitution = self.parse_expr(ctx, TokenType::BraceClose)?;
              self.require(TokenType::BraceClose)?;
              parts.push(LiteralTemplatePart::Substitution(substitution));
              let string = lex_template_string_continue(self.lexer_mut(), false)?;
              loc.extend(string.loc());
              parts.push(LiteralTemplatePart::String(string.loc().clone()));
              self.clear_buffered();
              match string.typ() {
                TokenType::LiteralTemplatePartStringEnd => break,
                _ => {}
              };
            }
            ctx.create_node(loc, Syntax::LiteralTemplateExpr { parts })
          }
          TokenType::LiteralTemplatePartStringEnd => {
            let mut parts = ctx.session.new_vec();
            parts.push(LiteralTemplatePart::String(t.loc().clone()));
            ctx.create_node(t.loc().clone(), Syntax::LiteralTemplateExpr { parts })
          }
          TokenType::LiteralUndefined => {
            ctx.create_node(t.loc().clone(), Syntax::LiteralUndefined {})
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
    ctx: ParseCtx<'a>,
    min_prec: u8,
    terminator_a: TokenType,
    terminator_b: TokenType,
    parenthesised: bool,
    asi: &mut Asi,
  ) -> SyntaxResult<'a, Node<'a>> {
    let mut left = self.parse_expr_operand(ctx, terminator_a, terminator_b, asi)?;

    loop {
      let cp = self.checkpoint();
      let t = self.next()?;

      if t.typ() == terminator_a || t.typ() == terminator_b {
        self.restore_checkpoint(cp);
        break;
      };

      match t.typ() {
        // Automatic Semicolon Insertion rules: no newline between operand and postfix operator.
        TokenType::PlusPlus | TokenType::HyphenHyphen if !t.preceded_by_line_terminator() => {
          let operator_name = match t.typ() {
            TokenType::PlusPlus => OperatorName::PostfixIncrement,
            TokenType::HyphenHyphen => OperatorName::PostfixDecrement,
            _ => unreachable!(),
          };
          let operator = &OPERATORS[&operator_name];
          if operator.precedence < min_prec {
            self.restore_checkpoint(cp);
            break;
          };
          left = ctx.create_node(left.loc() + t.loc(), Syntax::UnaryPostfixExpr {
            parenthesised: false,
            operator: operator_name,
            argument: left,
          });
          continue;
        }
        _ => {}
      };

      match MULTARY_OPERATOR_MAPPING.get(&t.typ()) {
        None => {
          if asi.can_end_with_asi
            && (t.preceded_by_line_terminator()
              || t.typ() == TokenType::BraceClose
              || t.typ() == TokenType::EOF)
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
              ctx.create_node(left.loc() + end.loc(), Syntax::CallExpr {
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
              ctx.create_node(left.loc() + end.loc(), Syntax::ComputedMemberExpr {
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
              ctx.create_node(left.loc() + alternate.loc(), Syntax::ConditionalExpr {
                parenthesised: false,
                test: left,
                consequent,
                alternate,
              })
            }
            OperatorName::MemberAccess | OperatorName::OptionalChainingMemberAccess => {
              let right_tok = self.next()?;
              match right_tok.typ() {
                TokenType::Identifier => {}
                TokenType::PrivateMember => {}
                t if KEYWORDS_MAPPING.contains_key(&t) => {}
                _ => {
                  return Err(
                    right_tok.error(SyntaxErrorType::ExpectedSyntax("member access property")),
                  )
                }
              };
              let right = right_tok.loc_take();
              ctx.create_node(left.loc() + right, Syntax::MemberExpr {
                parenthesised: false,
                optional_chaining: match operator.name {
                  OperatorName::OptionalChainingMemberAccess => true,
                  _ => false,
                },
                left,
                right,
              })
            }
            _ => {
              match operator.name {
                OperatorName::Assignment
                | OperatorName::AssignmentAddition
                | OperatorName::AssignmentBitwiseAnd
                | OperatorName::AssignmentBitwiseLeftShift
                | OperatorName::AssignmentBitwiseOr
                | OperatorName::AssignmentBitwiseRightShift
                | OperatorName::AssignmentBitwiseUnsignedRightShift
                | OperatorName::AssignmentBitwiseXor
                | OperatorName::AssignmentDivision
                | OperatorName::AssignmentExponentiation
                | OperatorName::AssignmentLogicalAnd
                | OperatorName::AssignmentLogicalOr
                | OperatorName::AssignmentMultiplication
                | OperatorName::AssignmentNullishCoalescing
                | OperatorName::AssignmentRemainder
                | OperatorName::AssignmentSubtraction => {
                  left = convert_assignment_lhs_to_target(ctx, left, operator.name)?;
                }
                _ => {}
              };
              let right = self.parse_expr_with_min_prec(
                ctx,
                next_min_prec,
                terminator_a,
                terminator_b,
                false,
                asi,
              )?;
              ctx.create_node(left.loc() + right.loc(), Syntax::BinaryExpr {
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
      match *left.stx_mut() {
        Syntax::ArrowFunctionExpr {
          ref mut parenthesised,
          ..
        }
        | Syntax::BinaryExpr {
          ref mut parenthesised,
          ..
        }
        | Syntax::CallExpr {
          ref mut parenthesised,
          ..
        }
        | Syntax::ConditionalExpr {
          ref mut parenthesised,
          ..
        }
        | Syntax::FunctionExpr {
          ref mut parenthesised,
          ..
        }
        | Syntax::MemberExpr {
          ref mut parenthesised,
          ..
        }
        | Syntax::UnaryExpr {
          ref mut parenthesised,
          ..
        } => {
          *parenthesised = true;
        }
        _ => {}
      };
    };

    Ok(left)
  }
}
