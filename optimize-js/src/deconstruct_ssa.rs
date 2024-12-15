use ahash::{HashMap, HashMapExt, HashSet};

use super::counter::Counter;
use super::inst::Inst;

pub fn deconstruct_ssa(
  bblocks: &mut HashMap<u32, Vec<Inst>>,
  cfg_parents: &mut HashMap<u32, HashSet<u32>>,
  cfg_children: &mut HashMap<u32, HashSet<u32>>,
  c_label: &mut Counter,
) {
  struct NewBblock {
    label: u32,
    parent: u32,
    child: u32,
    insts: Vec<Inst>,
  }
  let mut new_bblocks = Vec::<NewBblock>::new();
  for (&label, bblock) in bblocks.iter_mut() {
    let mut new_bblocks_by_parent = HashMap::<u32, NewBblock>::new();
    while bblock
      .first()
      .is_some_and(|i| matches!(i, Inst::Phi { .. }))
    {
      let Inst::Phi {
        tgt, from_blocks, ..
      } = bblock.remove(0)
      else {
        unreachable!();
      };
      for (parent, value) in from_blocks {
        new_bblocks_by_parent
          .entry(parent)
          .or_insert_with(|| NewBblock {
            label: c_label.bump(),
            parent,
            child: label,
            insts: Vec::new(),
          })
          .insts
          .push(Inst::VarAssign {
            tgt: tgt.clone(),
            value,
          });
      }
    }
    new_bblocks.extend(new_bblocks_by_parent.into_values());
  }
  for mut b in new_bblocks {
    // Detach parent from child.
    cfg_parents.get_mut(&b.child).unwrap().remove(&b.parent);
    cfg_children.get_mut(&b.parent).unwrap().remove(&b.child);
    // Update any goto inst in parent.
    match bblocks.get_mut(&b.parent).unwrap().last_mut() {
      Some(Inst::Goto { label })
      | Some(Inst::CondGoto { label, .. })
      | Some(Inst::NotCondGoto { label, .. }) => {
        *label = b.label;
      }
      _ => {}
    };
    // Attach new bblock.
    cfg_parents.get_mut(&b.child).unwrap().insert(b.label);
    cfg_children.get_mut(&b.parent).unwrap().insert(b.label);
    // Insert new bblock.
    b.insts.push(Inst::Goto { label: b.child });
    bblocks.insert(b.label, b.insts);
    cfg_parents.insert(b.label, HashSet::from_iter([b.parent]));
    cfg_children.insert(b.label, HashSet::from_iter([b.child]));
  }
}
