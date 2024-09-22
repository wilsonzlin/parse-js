pub(crate) mod backedge;
pub(crate) mod bblock;
pub(crate) mod builtin;
pub(crate) mod cfg;
pub(crate) mod consteval;
pub(crate) mod counter;
pub(crate) mod deconstruct_ssa;
pub(crate) mod defs;
pub(crate) mod domfront;
pub(crate) mod dominators;
pub(crate) mod domtree;
pub(crate) mod dot;
pub(crate) mod inst;
pub(crate) mod interference;
pub(crate) mod liveness;
pub(crate) mod optpass_cfg_prune;
pub(crate) mod optpass_dvn;
pub(crate) mod optpass_impossible_branches;
pub(crate) mod optpass_redundant_assigns;
pub(crate) mod optpass_trivial_dce;
pub(crate) mod postorder;
pub(crate) mod register_alloc;
pub(crate) mod single_use_insts;
pub(crate) mod source_to_inst;
pub(crate) mod ssi_insert_phis;
pub(crate) mod ssi_rename;
pub(crate) mod visit;
pub(crate) mod find_loops;

use std::env::var;

use ahash::{AHashMap, AHashSet};
use bblock::convert_insts_to_bblocks;
use cfg::calculate_cfg;
use counter::Counter;
use croaring::Bitmap;
use defs::calculate_defs;
use domfront::calculate_domfront;
use domtree::calculate_domtree;
use dot::render_cfg;
use inst::Inst;
use once_cell::sync::Lazy;
use optpass_dvn::optpass_dvn;
use optpass_impossible_branches::optpass_impossible_branches;
use optpass_redundant_assigns::optpass_redundant_assigns;
use optpass_trivial_dce::optpass_trivial_dce;
use parse_js::ast::Node;
use postorder::calculate_postorder;
use source_to_inst::translate_source_to_inst;
use ssi_insert_phis::insert_phis_for_ssi_construction;
use ssi_rename::rename_targets_for_ssi_construction;
use backedge::{find_backedges_and_junctions};
use deconstruct_ssa::deconstruct_ssa;
use interference::calculate_interference_graph;
use liveness::calculate_live_ins;
use optpass_cfg_prune::optpass_cfg_prune;
use register_alloc::allocate_registers;
use single_use_insts::analyse_single_use_defs;

static DEBUG_DOT: Lazy<AHashSet<String>> = Lazy::new(|| var("MJS_DEBUG_DOT").ok().map(|v| v.split(',').map(|s| s.to_string()).collect()).unwrap_or_default());

fn dbg_cfg(
  name: &str,
  bblock_order: &[u32],
  bblocks: &AHashMap<u32, Vec<Inst>>,
  cfg_children: &AHashMap<u32, Bitmap>,
) {
  if DEBUG_DOT.contains("1") || DEBUG_DOT.contains(name) {
    let out_dir = var("MJS_DEBUG_DOT_OUTDIR").unwrap_or_else(|_| ".".to_string());
    render_cfg(
      &format!("{out_dir}/minify-js_debug_cfg_{name}.png"),
      name,
      bblock_order,
      bblocks,
      cfg_children,
    );
  }
}

pub(crate) fn compile_js_statements(
  statements: &[Node],
) -> AHashMap<u32, Vec<Inst>> {
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
  dbg_cfg("0. Source", &postorder, &bblocks, &cfg_children);

  // Construct SSA.
  insert_phis_for_ssi_construction(&mut defs, &mut bblocks, &domfront);
  dbg_cfg("1. SSI (Insert Phis)", &postorder, &bblocks, &cfg_children);
  rename_targets_for_ssi_construction(&mut bblocks, &cfg_children, &domtree, &mut c_temp);
  dbg_cfg("1. SSI (Rename Targets)", &postorder, &bblocks, &cfg_children);

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
    optpass_trivial_dce(&mut changed, &mut bblocks);
    // TODO Isn't this really const/copy propagation to child Phi insts?
    optpass_redundant_assigns(&mut changed, &mut bblocks);
    optpass_impossible_branches(
      &mut changed,
      &mut bblocks,
      &mut cfg_parents,
      &mut cfg_children,
    );
    optpass_cfg_prune(
      &mut changed,
      &mut bblocks,
      &mut cfg_parents,
      &mut cfg_children,
    );

    if !changed {
      break;
    }
    dbg_cfg(&format!("2. Optimisation Pass ({i})"), &postorder, &bblocks, &cfg_children);
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

  // To calculate the post dominators, reverse the edges and run any dominator algorithm.
  // TODO Reenable.
  // let ipostdom_by = {
  //   let (rpo, rpo_label) = calculate_postorder(&cfg_parents, u32::MAX);
  //   calculate_domtree(&cfg_children, &rpo, &rpo_label, u32::MAX).0
  // };
  // let backedges = find_backedges_and_junctions(&cfg_children);

  bblocks
}
