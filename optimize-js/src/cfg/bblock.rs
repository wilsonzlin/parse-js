use ahash::{HashMap, HashMapExt};

use crate::{il::inst::{Inst, InstTyp}, util::counter::Counter};

pub type BBlock = Vec<Inst>;

// We may need to create some new blocks (which require labels), which is why we need `c_label`.
pub fn convert_insts_to_bblocks(
  insts: Vec<Inst>,
  c_label: &mut Counter,
) -> (HashMap<u32, Vec<Inst>>, Vec<u32>) {
  let mut bblocks = HashMap::<u32, Vec<Inst>>::new();
  // The order is required for implicit "fallthrough" jumps to the lexically "next" block.
  let mut bblock_order = vec![0];
  let mut bblock = bblocks.entry(0).or_default();
  for inst in insts {
    match inst.t {
      InstTyp::Label => {
        let label = inst.labels[0];
        bblock_order.push(label);
        bblock = bblocks.entry(label).or_default();
      }
      InstTyp::Goto | InstTyp::CondGoto => {
        bblock.push(inst);
        let l = c_label.bump();
        bblock_order.push(l);
        bblock = bblocks.entry(l).or_default();
      }
      _ => {
        bblock.push(inst);
      }
    };
  }
  (bblocks, bblock_order)
}
