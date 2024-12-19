pub mod cfg;
pub mod graph;
pub mod dom;
pub mod opt;
pub mod ssa;
pub mod symbol;
pub mod il;
pub mod util;
pub mod eval;
pub mod analysis;

use std::{ops::Deref, sync::{atomic::{AtomicUsize, Ordering}, Arc}};

use analysis::{defs::calculate_defs, interference::calculate_interference_graph, liveness::calculate_live_ins, register_alloc::allocate_registers, single_use_insts::analyse_single_use_defs};
use cfg::{bblock::convert_insts_to_bblocks, cfg::{Cfg, CfgBBlocks}};
use crossbeam_utils::sync::WaitGroup;
use dashmap::DashMap;
use dom::{domfront::calculate_domfront, domtree::calculate_domtree};
use graph::postorder::calculate_postorder;
use opt::{optpass_cfg_prune::optpass_cfg_prune, optpass_dvn::optpass_dvn, optpass_impossible_branches::optpass_impossible_branches, optpass_redundant_assigns::optpass_redundant_assigns, optpass_trivial_dce::optpass_trivial_dce};
use parse_js::ast::{Node, Syntax};
use serde::Serialize;
use ssa::{deconstruct_ssa::deconstruct_ssa, ssa_insert_phis::insert_phis_for_ssa_construction, ssa_rename::rename_targets_for_ssa_construction};
use symbol::var_visitor::VarAnalysis;
use util::{counter::Counter, debug::OptimizerDebug};

// The top level is considered a function (the optimizer concept, not parser or symbolizer).
#[derive(Debug, Serialize)]
pub struct ProgramFunction {
  pub debug: Option<OptimizerDebug>,
  pub body: CfgBBlocks,
}

pub fn compile_js_statements(
  program: &ProgramCompiler,
  statements: &[Node],
) -> ProgramFunction {
  let mut dbg = program.debug.then(|| OptimizerDebug::new());
  let mut dbg_checkpoint = |name: &str, cfg: &Cfg| {
    dbg.as_mut().map(|dbg| dbg.add_step(name, cfg));
  };

  // Label 0 is for entry.
  let mut c_label = Counter::new(1);
  let mut c_temp = Counter::new(0);
  let insts = program.translate_source_to_inst(statements, &mut c_label, &mut c_temp);
  let (bblocks, bblock_order) = convert_insts_to_bblocks(insts, &mut c_label);
  let mut cfg = Cfg::from_bblocks(bblocks, bblock_order);
  // Prune unreachable blocks from 0. This is necessary for dominance calculation to be correct (basic example: every block should be dominated by 0, but if there's an unreachable block it'll make all its descendants not dominated by 0).
  // This can happen due to user code (unreachable code) or by us, because we split after a `goto` which makes the new other-split-half block unreachable (this block is usually empty).
  cfg.find_and_pop_unreachable();

  let (postorder, label_to_postorder) = calculate_postorder(&cfg, 0);
  let (idom_by, domtree) = calculate_domtree(&cfg, &postorder, &label_to_postorder, 0);
  let domfront = calculate_domfront(&cfg, &idom_by, &postorder);
  let mut defs = calculate_defs(&cfg);
  dbg_checkpoint("source", &cfg);

  // Construct SSA.
  insert_phis_for_ssa_construction(&mut defs, &mut cfg, &domfront);
  dbg_checkpoint("ssa_insert_phis", &cfg);
  rename_targets_for_ssa_construction(&mut cfg, &domtree, &mut c_temp);
  dbg_checkpoint("ssa_rename_targets", &cfg);

  // Optimisation passes:
  // - Dominator-based value numbering.
  // - Trivial dead code elimination.
  // Drop defs as it likely will be invalid after even one pass.
  drop(defs);
  for i in 1.. {
    let mut changed = false;

    // TODO Can we avoid recalculating these on every iteration i.e. mutate in-place when changing the CFG?
    let (postorder, label_to_postorder) = calculate_postorder(&cfg, 0);
    let (_, domtree) = calculate_domtree(&cfg, &postorder, &label_to_postorder, 0);

    optpass_dvn(&mut changed, &mut cfg, &domtree);
    dbg_checkpoint(&format!("opt{}_dvn", i), &cfg);
    optpass_trivial_dce(&mut changed, &mut cfg);
    dbg_checkpoint(&format!("opt{}_dce", i), &cfg);
    // TODO Isn't this really const/copy propagation to child Phi insts?
    optpass_redundant_assigns(&mut changed, &mut cfg);
    dbg_checkpoint(&format!("opt{}_redundant_assigns", i), &cfg);
    optpass_impossible_branches(
      &mut changed,
      &mut cfg,
    );
    dbg_checkpoint(&format!("opt{}_impossible_branches", i), &cfg);
    optpass_cfg_prune(
      &mut changed,
      &mut cfg,
    );
    dbg_checkpoint(&format!("opt{}_cfg_prune", i), &cfg);

    if !changed {
      break;
    }
  }

  let (inlines, inlined_tgts) = analyse_single_use_defs(&cfg);
  let liveness = calculate_live_ins(
    &cfg,
    &inlines,
    &inlined_tgts,
  );
  let intgraph = calculate_interference_graph(&liveness);
  let var_alloc = allocate_registers(&intgraph);

  // It's safe to calculate liveliness before removing Phi insts; after deconstructing, they always lie exactly between all parent bblocks and the head of the bblock, so their lifetimes are identical.
  deconstruct_ssa(
    &mut cfg,
    &mut c_label,
  );
  dbg_checkpoint("ssa_deconstruct", &cfg);

  // To calculate the post dominators, reverse the edges and run any dominator algorithm.
  // TODO Reenable.
  // let ipostdom_by = {
  //   let (rpo, rpo_label) = calculate_postorder(&cfg_parents, u32::MAX);
  //   calculate_domtree(&cfg_children, &rpo, &rpo_label, u32::MAX).0
  // };
  // let backedges = find_backedges_and_junctions(&cfg_children);

  ProgramFunction {
    debug: dbg,
    body: cfg.bblocks,
  }
}

pub type FnId = usize;

#[derive(Debug)]
pub struct ProgramCompilerInner {
  pub functions: DashMap<FnId, ProgramFunction>,
  pub next_fn_id: AtomicUsize,
  pub debug: bool,
  pub wg: WaitGroup,
}

/// Our internal compiler state for a program.
/// We have a separate struct instead of using the public-facing Program.
/// It means we can use pub instead of pub(crate) fields and methods everywhere.
#[derive(Clone)]
pub struct ProgramCompiler(Arc<ProgramCompilerInner>);

impl Deref for ProgramCompiler {
  type Target = ProgramCompilerInner;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

pub struct Program {
  pub functions: Vec<ProgramFunction>,
  pub top_level: ProgramFunction,
}

impl Program {
  // The AST must already have symbol analysis done by compute_symbols.
  pub fn compile(top_level_node: &Node, debug: bool) -> Self {
    let VarAnalysis {
      declared,
      foreign,
      unknown,
      use_before_decl,
    } = VarAnalysis::analyze(&top_level_node);
    // SSA requires no use before declaration.
    if let Some((_, loc)) = use_before_decl.iter().next() {
      panic!("Use before declaration at {:?}", loc);
    };
    let Syntax::TopLevel { body } = top_level_node.stx.as_ref() else {
      panic!();
    };
    let program = ProgramCompiler(Arc::new(ProgramCompilerInner {
      functions: DashMap::new(),
      next_fn_id: AtomicUsize::new(0),
      debug,
      wg: WaitGroup::new(),
    }));
    let top_level = compile_js_statements(&program, body);
    let ProgramCompilerInner {
      functions,
      next_fn_id,
      wg,
      ..
    } = Arc::try_unwrap(program.0).unwrap();
    wg.wait();
    let fn_count = next_fn_id.load(Ordering::Relaxed);
    Self {
      functions: (0..fn_count).map(|i| functions.remove(&i).unwrap().1).collect(),
      top_level,
    }
  }
}

#[cfg(test)]
mod tests {
    use parse_js::{ast::Syntax, parse};
    use symbol_js::{compute_symbols, TopLevelMode};

    use crate::{Program};

  #[test]
  fn test_compile_js_statements() {
    let source = r#"
      a?.b?.c;
      let x = 1;
      if (x) {
        g();
        x += 1;
        for (;;) {
          x += 1;
        }
      }
      f(x);
    "#;
    let mut top_level_node = parse(source.as_bytes()).expect("parse input");
    compute_symbols(&mut top_level_node, TopLevelMode::Module);
    let bblocks = Program::compile(&top_level_node, false).top_level;
  }
}
