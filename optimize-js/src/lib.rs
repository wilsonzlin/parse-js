// TODO Remove
#![allow(unused)]

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

use ahash::HashMap;
use analysis::{defs::calculate_defs, interference::calculate_interference_graph, liveness::calculate_live_ins, register_alloc::allocate_registers, single_use_insts::analyse_single_use_defs};
use cfg::{bblock::convert_insts_to_bblocks, cfg::{Cfg, CfgBBlocks}};
use dom::{domfront::calculate_domfront, domtree::calculate_domtree};
use graph::postorder::calculate_postorder;
use il::{inst::Inst, source_to_inst::translate_source_to_inst};
use itertools::Itertools;
use once_cell::sync::Lazy;
use opt::{optpass_cfg_prune::optpass_cfg_prune, optpass_dvn::optpass_dvn, optpass_impossible_branches::optpass_impossible_branches, optpass_redundant_assigns::optpass_redundant_assigns, optpass_trivial_dce::optpass_trivial_dce};
use parse_js::ast::Node;
use ssa::{deconstruct_ssa::deconstruct_ssa, ssa_insert_phis::insert_phis_for_ssa_construction, ssa_rename::rename_targets_for_ssa_construction};
use util::{counter::Counter, debug::OptimizerDebug};

pub fn compile_js_statements(
  statements: &[Node],
  mut dbg: Option<&mut OptimizerDebug>,
) -> CfgBBlocks {
  // Label 0 is for entry.
  let mut c_label = Counter::new(1);
  let mut c_temp = Counter::new(0);
  let insts = translate_source_to_inst(statements, &mut c_label, &mut c_temp);
  let (bblocks, bblock_order) = convert_insts_to_bblocks(insts, &mut c_label);
  let mut cfg = Cfg::from_bblocks(bblocks, bblock_order);
  // Prune unreachable blocks from 0. This is necessary for dominance calculation to be correct (basic example: every block should be dominated by 0, but if there's an unreachable block it'll make all its descendants not dominated by 0).
  // This can happen due to user code (unreachable code) or by us, because we split after a `goto` which makes the new other-split-half block unreachable (this block is usually empty).
  cfg.find_and_pop_unreachable();

  let (postorder, label_to_postorder) = calculate_postorder(&cfg, 0);
  let (idom_by, domtree) = calculate_domtree(&cfg, &postorder, &label_to_postorder, 0);
  let domfront = calculate_domfront(&cfg, &idom_by, &postorder);
  let mut defs = calculate_defs(&cfg);
  dbg.as_mut().map(|dbg| dbg.add_step("source", &cfg));

  // Construct SSA.
  insert_phis_for_ssa_construction(&mut defs, &mut cfg, &domfront);
  dbg.as_mut().map(|dbg| dbg.add_step("ssa_insert_phis", &cfg));
  rename_targets_for_ssa_construction(&mut cfg, &domtree, &mut c_temp);
  dbg.as_mut().map(|dbg| dbg.add_step("ssa_rename_targets", &cfg));

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
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_dvn", i), &cfg));
    optpass_trivial_dce(&mut changed, &mut cfg);
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_dce", i), &cfg));
    // TODO Isn't this really const/copy propagation to child Phi insts?
    optpass_redundant_assigns(&mut changed, &mut cfg);
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_redundant_assigns", i), &cfg));
    optpass_impossible_branches(
      &mut changed,
      &mut cfg,
    );
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_impossible_branches", i), &cfg));
    optpass_cfg_prune(
      &mut changed,
      &mut cfg,
    );
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_cfg_prune", i), &cfg));

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
  dbg.as_mut().map(|dbg| dbg.add_step("ssa_deconstruct", &cfg));

  // To calculate the post dominators, reverse the edges and run any dominator algorithm.
  // TODO Reenable.
  // let ipostdom_by = {
  //   let (rpo, rpo_label) = calculate_postorder(&cfg_parents, u32::MAX);
  //   calculate_domtree(&cfg_children, &rpo, &rpo_label, u32::MAX).0
  // };
  // let backedges = find_backedges_and_junctions(&cfg_children);

  cfg.bblocks
}

#[cfg(test)]
mod tests {
    use parse_js::{ast::Syntax, parse};
    use symbol_js::{compute_symbols, TopLevelMode};

    use crate::compile_js_statements;

  #[test]
  fn test_compile_js_statements() {
    let source = r#"
      let x = 1;
      if (x) {
        g();
      }
      f(x);
    "#;
    let mut top_level_node = parse(source.as_bytes()).expect("parse input");
    compute_symbols(&mut top_level_node, TopLevelMode::Module);
    let Syntax::TopLevel { body } = top_level_node.stx.as_ref() else {
      panic!();
    };
    let bblocks = compile_js_statements(body, None);
  }
}
