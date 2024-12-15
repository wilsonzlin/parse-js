use std::collections::VecDeque;

use ahash::{HashMap, HashMapExt, HashSet};


// - Given backedge from B -> A, A is the loop header.
// - Multiple backedges may point to the same tail (i.e. loop header) but have different heads.
//   - Consider: `continue` statements.
// - A backedge may point to a loop header outside its own loop.
//   - Consider: labelled `continue` statements.
// - A loop may be inside another loop; therefore, nodes may be part of multiple loops.
pub fn find_loops(
  cfg_parents: &HashMap<u32, HashSet<u32>>,
  backedges: &HashMap<u32, u32>, // Map from A -> B where B -> A is a backedge.
) -> HashMap<u32, HashSet<u32>> {
  // Map from header -> nodes (including the header).
  let mut loops = HashMap::<u32, HashSet<u32>>::new();
  for (&header, &b) in backedges {
    let l = loops.entry(header).or_default();
    l.insert(header);
    l.insert(b);
    let mut queue = VecDeque::from([b]);
    let mut seen = HashSet::from_iter([header, b]);
    while let Some(n) = queue.pop_front() {
      // We have reducible CFGs, so all parents must be part of the loop; they can't magically enter from outside the loop.
      for &p in cfg_parents[&n].iter() {
        if seen.contains(&p) {
          continue;
        };
        seen.insert(p);
        l.insert(p);
        queue.push_back(p);
      };
    };
  };
  loops
}
