use ahash::{HashMap, HashSet, HashSetExt};

use crate::cfg::cfg::Cfg;

struct PostOrderVisitor<'a> {
  cfg: &'a Cfg,
  seen: HashSet<u32>,
  order: Vec<u32>,
}

impl<'a> PostOrderVisitor<'a> {
  fn visit(&mut self, n: u32) {
    if self.seen.contains(&n) {
      return;
    };
    self.seen.insert(n);
    for c in self.cfg.graph.children(n) {
      self.visit(c);
    }
    self.order.push(n);
  }
}

// Postorder: visit all children, then self.
pub fn calculate_postorder(
  cfg: &Cfg,
  entry: u32,
) -> (Vec<u32>, HashMap<u32, usize>) {
  let mut order_po_v = PostOrderVisitor {
    cfg,
    order: Vec::new(),
    seen: HashSet::new(),
  };
  order_po_v.visit(entry);
  // Order of blocks to visit in order to visit by postorder. Elements are labels. This can also be used to map from postorder number (i.e. number each node would be assigned if sequentially visited and assigned in postorder) to label.
  let order_po = order_po_v.order;
  // Map from postorder number to label (see above).
  let label_to_po = order_po
    .iter()
    .enumerate()
    .map(|(i, l)| (*l, i))
    .collect::<HashMap<_, _>>();
  (order_po, label_to_po)
}
