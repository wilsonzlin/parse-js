use ahash::AHashMap;
use parse_js::{ast::{Node, Syntax}, loc::Loc, operator::OperatorName};

use crate::minify::function::inst::{Arg, BinOp, Const};

use super::function::inst::Inst;

// Reconstruction Representation.
pub(crate) enum Rr {

}

fn reconstruct_bblocks(cfg: &AHashMap<u32, Vec<Inst>>) -> Node {
  fn arg_to_expr(arg: &Arg) -> Node {
    let stx = match arg {
      Arg::Builtin(v) => todo!(),
      Arg::Const(v) => match v {
        Const::BigInt(v) => todo!(),
        Const::Bool(v) => todo!(),
        Const::Null => Syntax::LiteralNull {  },
        Const::Num(v) => Syntax::LiteralNumberExpr { value: *v },
        Const::Str(v) => todo!(),
        Const::Undefined => todo!(),
      }
      // TODO
      Arg::Var(v) => Syntax::IdentifierExpr { name: format!("tmp{v}") },
    };
    Node::new(Loc(0, 0), stx)
  };
  let bblock = &cfg[&0];
  let mut stmts = Vec::new();
  for inst in bblock {
    let stx = match inst {
      Inst::Bin { tgt, left, op, right } => {
        let operator = match op {
          BinOp::Add => OperatorName::Addition,
          BinOp::Div => OperatorName::Division,
          BinOp::Exp => OperatorName::Exponentiation,
          BinOp::Geq => OperatorName::GreaterThanOrEqual,
          BinOp::GetProp => todo!(),
          BinOp::Gt => OperatorName::GreaterThan,
          BinOp::Leq => OperatorName::LessThanOrEqual,
          BinOp::LooseEq => OperatorName::Equality,
          BinOp::Lt => OperatorName::LessThan,
          BinOp::Mod => OperatorName::Remainder,
          BinOp::Mul => OperatorName::Multiplication,
          BinOp::NotLooseEq => OperatorName::Inequality,
          BinOp::NotStrictEq => OperatorName::StrictInequality,
          BinOp::StrictEq => OperatorName::StrictEquality,
          BinOp::Sub => OperatorName::Subtraction,
        };
        Syntax::BinaryExpr {
          left: arg_to_expr(left),
          operator,
          parenthesised: false, // TODO
          right: arg_to_expr(right),
        }
      }
      _ => unreachable!(),
    };
    let n_expr = Node::new(Loc(0, 0), stx);
    let n_stmt = Node::new(Loc(0, 0), Syntax::ExpressionStmt { expression: n_expr });
    stmts.push(n_stmt);
  };
  Node::new(Loc(0, 0), Syntax::TopLevel { body: stmts })
}

#[cfg(test)]
mod tests {
    use ahash::AHashMap;

    use crate::{emit::emit_js, minify::function::inst::{Arg, BinOp, Inst}};
    use crate::minify::function::inst::Const;
    use parse_js::num::JsNumber;

    use super::reconstruct_bblocks;

  #[test]
  fn test_reconstruction() {
    #[rustfmt::skip]
    let bblocks = AHashMap::from([
      (0, vec![
        Inst::Bin { tgt: 1, left: Arg::Var(0), op: BinOp::Add, right: Arg::Var(0) },
        Inst::Bin { tgt: 1, left: Arg::Var(0), op: BinOp::Add, right: Arg::Const(Const::Num(JsNumber(3.14))) },
      ]),
    ]);
    let ast = reconstruct_bblocks(&bblocks);
    let mut out = Vec::new();
    emit_js(&mut out, &ast);
    println!("{}", String::from_utf8(out).unwrap());
  }
}
