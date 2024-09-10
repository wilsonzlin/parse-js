use ahash::AHashMap;
use parse_js::{ast::{Node, Syntax}, loc::Loc};

use super::inst::{Inst, Arg, Const};

fn n(stx: Syntax) -> Node {
  Node::new(Loc(0, 0), stx)
}

fn arg(a: &Arg) -> Node {
  match a {
    Arg::Builtin(_) => todo!(),
    Arg::Const(v) => match v {
      Const::BigInt(v) => todo!(),
      Const::Bool(_) => todo!(),
      Const::Null => todo!(),
      Const::Num(_) => todo!(),
      Const::Str(_) => todo!(),
      Const::Undefined => todo!(),
    }
    Arg::Var(v) => todo!(),
  }
}

fn reconstruct_bblock(
  bblocks: &AHashMap<u32, Vec<Inst>>,
  bblock: u32,
) {
  // let mut seq_expr = None;
  for inst in bblocks[&bblock].iter() {
    let e = match inst {
      Inst::Bin { tgt, left, op, right } => todo!(),
      Inst::Un { tgt, arg, op } => todo!(),
      Inst::VarAssign { tgt, value } => todo!(),
      Inst::PropAssign { obj, prop, value } => todo!(),
      Inst::Goto { label } => todo!(),
      Inst::CondGoto { cond, label } => todo!(),
      Inst::NotCondGoto { cond, label } => todo!(),
      Inst::Call { tgt, func, this, args } => todo!(),
      Inst::ForeignLoad { from, to } => todo!(),
      Inst::ForeignStore { from, to } => todo!(),
      Inst::UnknownLoad { from, to } => todo!(),
      Inst::UnknownStore { from, to } => todo!(),
      Inst::Phi { tgt, from_blocks } => todo!(),
      Inst::Label { label } => todo!(),
    };
  }
  todo!()
}

pub(crate) fn reconstruct_source(
  bblocks: &AHashMap<u32, Vec<Inst>>,
) {

}
