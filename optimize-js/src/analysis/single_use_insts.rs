use ahash::HashMap;
use ahash::HashMapExt;
use ahash::HashSet;
use ahash::HashSetExt;

use crate::cfg::cfg::Cfg;
use crate::il::inst::Arg;

pub fn analyse_single_use_defs(
  cfg: &Cfg,
) -> (HashMap<(u32, usize), (u32, usize)>, HashSet<u32>) {
  let mut use_locs = HashMap::<u32, Vec<(u32, usize)>>::new();
  let mut def_locs = HashMap::<u32, (u32, usize)>::new();
  for (label, insts) in cfg.bblocks.all() {
    for (inst_no, inst) in insts.iter().enumerate() {
      for arg in inst.args.iter() {
        match arg {
          Arg::Var(t) => use_locs.entry(*t).or_default().push((label, inst_no)),
          _ => {}
        }
      }
      for &var in inst.tgts.iter() {
        assert!(def_locs.insert(var, (label, inst_no)).is_none())
      }
    }
  }

  // K => V, where the inst at location K has been inlined into location V. Location V may itself be inlined into somewhere else.
  let mut inlines = HashMap::<(u32, usize), (u32, usize)>::new();
  let mut inlined_vars = HashSet::new();
  for (var, uses) in use_locs {
    if uses.len() != 1 {
      continue;
    };
    let dest_loc = uses.first().unwrap();
    let src_loc = def_locs.remove(&var).unwrap();
    // Inlining is only supported within the same bblock.
    if src_loc.0 != dest_loc.0 {
      continue;
    };
    assert!(src_loc.1 < dest_loc.1);
    assert!(inlines.insert(src_loc, dest_loc.clone()).is_none());
    inlined_vars.insert(var);
  }

  (inlines, inlined_vars)
}
