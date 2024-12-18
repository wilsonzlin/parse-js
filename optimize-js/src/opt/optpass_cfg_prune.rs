use ahash::{HashMap, HashSet};
use itertools::Itertools;

use crate::{cfg::cfg::Cfg, il::inst::{Inst, InstTyp}};

/**
 * WARNING: Read comment in cfg.rs.
 */

/// Remove bblocks that are the only child of their parent.
pub fn optpass_cfg_prune(
  changed: &mut bool,
  cfg: &mut Cfg,
) {
  // Iterate until convergence, instead of waiting for another optimisation pass.
  loop {
    // WARNING: We must update graph within this loop, instead of simply marking and then removing afterwards, as we possibly pop instructions which could make a non-empty bblock empty, but if we don't then immediately update the graph some invariants won't hold (e.g. empty bblocks have <= 1 children). This means we can't use common utility graph functions.
    let mut converged = true;
    for cur in cfg.graph.labels().collect_vec() {
      // TODO Figure out how to delete node 0 (i.e. re-root).
      if cur == 0 {
        continue;
      };
      let Ok(parent) = cfg.graph.parents(cur).exactly_one() else {
        continue;
      };
      if cfg.graph.children(parent).count() != 1 {
        continue;
      };
      // Clone so we can update other entries in `cfg_*`.
      let children = cfg.graph.children(cur).collect_vec();

      // Connect parent to children.
      for &c in children.iter() {
        cfg.graph.connect(parent, c);
      }
      // Detach from children.
      for &c in children.iter() {
        cfg.graph.disconnect(cur, c);
      }
      // Detach from parent.
      cfg.graph.disconnect(parent, cur);

      // The parent has exactly one child, so it cannot have a CondGoto.
      // Therefore, it must have zero or one Goto.
      let p_bblock = cfg.bblocks.get_mut(parent);
      if let Some(Inst { t: InstTyp::Goto, labels, .. }) = p_bblock.last() {
        assert!(labels.contains(&cur));
        p_bblock.pop().unwrap();
      }
      // Detach.
      let mut insts = cfg.bblocks.remove(cur);
      cfg.graph.pop(cur);
      // Move insts to parent.
      cfg.bblocks.get_mut(parent).append(&mut insts);
      // Update phi nodes in children.
      for &c in children.iter() {
        for c_inst in cfg.bblocks.get_mut(c) {
          if c_inst.t != InstTyp::Phi {
            // No more phi nodes.
            break;
          };
          if let Some(ex) = c_inst.remove_phi(cur) {
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
