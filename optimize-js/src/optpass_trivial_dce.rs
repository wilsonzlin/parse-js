use ahash::HashMap;
use ahash::HashSet;
use ahash::HashSetExt;

use super::inst::Arg;
use super::inst::CallArg;
use super::inst::Inst;
use super::visit::visit_inst_args;
use super::visit::visit_inst_tgts;

pub fn optpass_trivial_dce(changed: &mut bool, bblocks: &mut HashMap<u32, Vec<Inst>>) {
  let mut used = HashSet::new();
  for bblock in bblocks.values() {
    for inst in bblock.iter() {
      visit_inst_args!(inst, |arg| match arg {
        Arg::Var(t) => {
          used.insert(*t);
        }
        _ => {}
      });
    }
  }
  for bblock in bblocks.values_mut() {
    for i in (0..bblock.len()).rev() {
      let mut should_delete = false;
      visit_inst_tgts!(&mut bblock[i], |var| should_delete = !used.contains(var));
      if should_delete {
        if let Inst::Call { tgt, .. } = &mut bblock[i] {
          // We pessimistically assume all functions are non-pure and therefore cannot be removed. Therefore, we can only remove the target.
          tgt.take().unwrap();
        } else {
          bblock.remove(i);
        };
        *changed = true;
      };
    }
  }
}
