use ahash::{HashMap, HashSet};
use itertools::Itertools;

use crate::{cfg::cfg::Cfg, il::inst::{Inst, InstTyp}};

/**
 * WARNING: Read comment in cfg.rs.
 */

pub fn optpass_cfg_prune(
  changed: &mut bool,
  cfg: &mut Cfg,
) {
  // Iterate until convergence, instead of waiting for another optimisation pass.
  loop {
    // WARNING: We must update graph within this loop, instead of simply marking and then removing afterwards, as we possibly pop instructions which could make a non-empty bblock empty, but if we don't then immediately update the graph some invariants won't hold (e.g. empty bblocks have <= 1 children). This means we can't use common utility graph functions.
    let mut converged = true;
    for label in cfg.graph.labels().collect_vec() {
      // TODO Figure out how to delete node 0 (i.e. re-root).
      if label == 0 {
        continue;
      };
      let Ok(parent) = cfg.graph.parents(label).exactly_one() else {
        continue;
      };
      if cfg.graph.children(parent).count() != 1 {
        continue;
      };
      // Clone so we can update other entries in `cfg_*`.
      let children = cfg.graph.children(label).collect_vec();

      // Connect parent to children.
      for &c in children.iter() {
        cfg.graph.connect(parent, c);
      }
      // Detach from children.
      for &c in children.iter() {
        cfg.graph.disconnect(label, c);
      }
      // Detach from parents.
      cfg.graph.disconnect(parent, label);

      // Remove any goto (cond. or otherwise) instruction in the parent block.
      let p_bblock = cfg.bblocks.get_mut(parent);
      if let Some(Inst { t: InstTyp::Goto | InstTyp::CondGoto, labels, .. }) = p_bblock.last() {
        assert_eq!(labels[0], label);
        p_bblock.pop();
      };
      // Detach.
      let mut insts = cfg.bblocks.remove(label);
      cfg.graph.pop(label);
      // Move insts to parent.
      cfg.bblocks.get_mut(parent).append(&mut insts);
      // Update phi nodes in children.
      for &c in children.iter() {
        for c_inst in cfg.bblocks.get_mut(c) {
          if c_inst.t != InstTyp::Phi {
            // No more phi nodes.
            break;
          };
          if let Some(ex) = c_inst.remove_phi(label) {
            c_inst.insert_phi(parent, ex);
          };
        }
      }
      *changed = true;
      converged = false;
    }

    if converged {
      break;
    }
  }
}
