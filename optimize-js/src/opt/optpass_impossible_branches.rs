use ahash::HashMap;
use ahash::HashSet;
use ahash::HashSetExt;
use itertools::Itertools;

use crate::cfg::cfg::Cfg;
use crate::eval::consteval::coerce_to_bool;
use crate::il::inst::Arg;
use crate::il::inst::Inst;
use crate::il::inst::InstTyp;

// Correctness:
// - When we detach bblocks A and B (because A can never branch to B in reality e.g. const eval is always true/false), we move all bblocks in subgraph G, which contains all bblocks only reachable from B.
// - We must then detach all bblocks within G i.e. remove all edges to bblocks outside of G. This isn't recursive, as the bblocks only reachable from B doesn't change as we remove these bblocks or their edges.
// - We must clean up any usages of defs within G outside of G. Outside of G, these uses can only appear in Phi nodes.
pub fn optpass_impossible_branches(
  changed: &mut bool,
  cfg: &mut Cfg,
) {
  loop {
    for label in cfg.graph.labels().collect_vec() {
      let Some(inst) = cfg.bblocks.get(label).last() else {
        continue;
      };
      let children = cfg.graph.children(label).collect_vec();
      if children.len() != 2 {
        continue;
      };
      if inst.t != InstTyp::CondGoto {
        continue;
      };
      let (Arg::Const(cond), true_label, false_label) = inst.as_cond_goto() else {
        continue;
      };
      let dead_child = if coerce_to_bool(cond) {
        false_label
      } else {
        true_label
      };
      // Remove instruction.
      cfg.bblocks.get_mut(label).pop().unwrap();
      // Detach from child.
      cfg.graph.disconnect(label, dead_child);
    }

    // Detaching from bblocks means that we may have removed entire subgraphs (i.e. other bblocks). Therefore, we must recalculate again the accessible bblocks.
    let to_delete = cfg.graph.find_unreachable().collect_vec();
    // All defs in now-deleted bblocks must be cleared. Since we are in strict SSA, they should only ever appear outside of the deleted bblocks in Phi insts.
    for &n in to_delete.iter() {
      // Update Phi insts in children.
      for c in cfg.graph.children(n) {
        for inst in cfg.bblocks.get_mut(c).iter_mut() {
          if inst.t != InstTyp::Phi {
            // No more Phi insts.
            break;
          };
          // NOTE: We don't try to remove the Phi insts or transform into a VarAssign (if it only has one entry in `from_blocks`) right now out of abundance of caution for correctness, since `from_blocks` could still be modified during these loops.
          inst.remove_phi(n);
        }
      }
    }

    // Delete bblocks now so that only valid bblocks remain, which is the set of bblocks to iterate for pruning Phi insts.
    let did_delete = !to_delete.is_empty();
    cfg.graph.delete_many(to_delete);

    // Prune Phi insts in remaining bblocks.
    for (_, bblock) in cfg.bblocks.all_mut() {
      let mut phis_to_delete = Vec::new();
      for (i, inst) in bblock.iter_mut().enumerate() {
        if inst.t != InstTyp::Phi {
          // No more Phi insts.
          break;
        };
        let tgt = inst.tgts[0];
        if inst.labels.is_empty() {
          // TODO Is this always safe?
          phis_to_delete.push(i);
        }
        if inst.labels.len() == 1 {
          let arg = inst.args[0].clone();
          *inst = Inst::var_assign(tgt, arg);
        };
      }
      for i in phis_to_delete.into_iter().rev() {
        bblock.remove(i);
      }
    }

    if !did_delete {
      break;
    }
    *changed = true;
  }
}