pub mod backedge;
pub mod bblock;
pub mod builtin;
pub mod cfg;
pub mod consteval;
pub mod counter;
pub mod deconstruct_ssa;
pub mod defs;
pub mod domfront;
pub mod dominators;
pub mod domtree;
pub mod inst;
pub mod interference;
pub mod liveness;
pub mod optpass_cfg_prune;
pub mod optpass_dvn;
pub mod optpass_impossible_branches;
pub mod optpass_redundant_assigns;
pub mod optpass_trivial_dce;
pub mod postorder;
pub mod register_alloc;
pub mod single_use_insts;
pub mod source_to_inst;
pub mod ssa_insert_phis;
pub mod ssa_rename;
pub mod visit;
pub mod find_loops;
pub mod debug;
pub mod var_visitor;

use ahash::{AHashMap, AHashSet, HashMap};
use bblock::convert_insts_to_bblocks;
use cfg::calculate_cfg;
use counter::Counter;
use debug::OptimizerDebug;
use defs::calculate_defs;
use domfront::calculate_domfront;
use domtree::calculate_domtree;
use inst::Inst;
use once_cell::sync::Lazy;
use optpass_dvn::optpass_dvn;
use optpass_impossible_branches::optpass_impossible_branches;
use optpass_redundant_assigns::optpass_redundant_assigns;
use optpass_trivial_dce::optpass_trivial_dce;
use parse_js::ast::Node;
use postorder::calculate_postorder;
use source_to_inst::translate_source_to_inst;
use ssa_insert_phis::insert_phis_for_ssa_construction;
use ssa_rename::rename_targets_for_ssa_construction;
use backedge::{find_backedges_and_junctions};
use deconstruct_ssa::deconstruct_ssa;
use interference::calculate_interference_graph;
use liveness::calculate_live_ins;
use optpass_cfg_prune::optpass_cfg_prune;
use register_alloc::allocate_registers;
use single_use_insts::analyse_single_use_defs;

pub fn compile_js_statements(
  statements: &[Node],
  mut dbg: Option<&mut OptimizerDebug>,
) -> HashMap<u32, Vec<Inst>> {
  // Label 0 is for entry.
  let mut c_label = Counter::new(1);
  let mut c_temp = Counter::new(0);
  let insts = translate_source_to_inst(statements, &mut c_label, &mut c_temp);
  let (mut bblocks, bblock_order) = convert_insts_to_bblocks(insts, &mut c_label);
  let (mut cfg_parents, mut cfg_children) = calculate_cfg(&mut bblocks, bblock_order);

  let (postorder, label_to_postorder) = calculate_postorder(&cfg_children, 0);
  let (idom_by, domtree) = calculate_domtree(&cfg_parents, &postorder, &label_to_postorder, 0);
  let domfront = calculate_domfront(&cfg_parents, &idom_by, &postorder);
  let mut defs = calculate_defs(&bblocks);
  dbg.as_mut().map(|dbg| dbg.add_step("source", &bblocks, &cfg_children));

  // Construct SSA.
  insert_phis_for_ssa_construction(&mut defs, &mut bblocks, &domfront);
  dbg.as_mut().map(|dbg| dbg.add_step("ssa_insert_phis", &bblocks, &cfg_children));
  rename_targets_for_ssa_construction(&mut bblocks, &cfg_children, &domtree, &mut c_temp);
  dbg.as_mut().map(|dbg| dbg.add_step("ssa_rename_targets", &bblocks, &cfg_children));

  // Optimisation passes:
  // - Dominator-based value numbering.
  // - Trivial dead code elimination.
  // Drop defs as it likely will be invalid after even one pass.
  drop(defs);
  for i in 1.. {
    let mut changed = false;

    // TODO Can we avoid recalculating these on every iteration i.e. mutate in-place when changing the CFG?
    let (postorder, label_to_postorder) = calculate_postorder(&cfg_children, 0);
    let (_, domtree) = calculate_domtree(&cfg_parents, &postorder, &label_to_postorder, 0);

    optpass_dvn(&mut changed, &mut bblocks, &cfg_children, &domtree);
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_dvn", i), &bblocks, &cfg_children));
    optpass_trivial_dce(&mut changed, &mut bblocks);
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_dce", i), &bblocks, &cfg_children));
    // TODO Isn't this really const/copy propagation to child Phi insts?
    optpass_redundant_assigns(&mut changed, &mut bblocks);
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_redundant_assigns", i), &bblocks, &cfg_children));
    optpass_impossible_branches(
      &mut changed,
      &mut bblocks,
      &mut cfg_parents,
      &mut cfg_children,
    );
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_impossible_branches", i), &bblocks, &cfg_children));
    optpass_cfg_prune(
      &mut changed,
      &mut bblocks,
      &mut cfg_parents,
      &mut cfg_children,
    );
    dbg.as_mut().map(|dbg| dbg.add_step(&format!("opt{}_cfg_prune", i), &bblocks, &cfg_children));

    if !changed {
      break;
    }
  }

  let (inlines, inlined_tgts) = analyse_single_use_defs(&bblocks);
  let liveness = calculate_live_ins(
    &bblocks,
    &cfg_parents,
    &cfg_children,
    &inlines,
    &inlined_tgts,
  );
  let intgraph = calculate_interference_graph(&liveness);
  let var_alloc = allocate_registers(&intgraph);

  // It's safe to calculate liveliness before removing Phi insts; after deconstructing, they always lie exactly between all parent bblocks and the head of the bblock, so their lifetimes are identical.
  deconstruct_ssa(
    &mut bblocks,
    &mut cfg_parents,
    &mut cfg_children,
    &mut c_label,
  );
  dbg.as_mut().map(|dbg| dbg.add_step("ssa_deconstruct", &bblocks, &cfg_children));

  // To calculate the post dominators, reverse the edges and run any dominator algorithm.
  // TODO Reenable.
  // let ipostdom_by = {
  //   let (rpo, rpo_label) = calculate_postorder(&cfg_parents, u32::MAX);
  //   calculate_domtree(&cfg_children, &rpo, &rpo_label, u32::MAX).0
  // };
  // let backedges = find_backedges_and_junctions(&cfg_children);

  bblocks
}
