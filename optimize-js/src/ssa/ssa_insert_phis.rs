use ahash::{HashMap, HashMapExt, HashSet, HashSetExt};
use itertools::Itertools;

use std::collections::VecDeque;

use crate::{cfg::cfg::Cfg, il::inst::Inst};

pub fn insert_phis_for_ssa_construction(
  defs: &mut HashMap<u32, HashSet<u32>>,
  cfg: &mut Cfg,
  domfront: &HashMap<u32, HashSet<u32>>,
) {
  for v in defs.keys().cloned().collect_vec() {
    let mut already_inserted = HashSet::new();
    // We'll start with these blocks but add more as we process, so we can't just use `defs[v].iter()`.
    let mut q = VecDeque::from_iter(defs[&v].clone());
    let mut seen = HashSet::from_iter(q.clone());
    while let Some(d) = q.pop_front() {
      // Look at the blocks in the dominance frontier for block `d`.
      let Some(blocks) = domfront.get(&d) else {
        continue;
      };
      for &label in blocks.iter() {
        if already_inserted.contains(&label) {
          continue;
        };
        already_inserted.insert(label);
        // We'll populate this new Phi inst later.
        cfg.bblocks.get_mut(label).insert(0, Inst::phi_empty(v));
        defs.get_mut(&v).unwrap().insert(label);
        if seen.insert(label) {
          q.push_back(label);
        };
      }
    }
  }
}
