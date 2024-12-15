use super::inst::Inst;
use super::visit::visit_inst_tgts;
use ahash::{HashMap, HashMapExt, HashSet};

// Find which bblocks assign to what vars.
pub fn calculate_defs(bblocks: &HashMap<u32, Vec<Inst>>) -> HashMap<u32, HashSet<u32>> {
  let mut defs = HashMap::<u32, HashSet<u32>>::new();
  for (label, insts) in bblocks.iter() {
    for inst in insts.iter() {
      visit_inst_tgts!(inst, |var| defs.entry(*var).or_default().insert(*label));
    }
  }
  defs
}
