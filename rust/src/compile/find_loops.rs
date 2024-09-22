use std::collections::VecDeque;

use ahash::AHashMap;
use croaring::Bitmap;

// - Given backedge from B -> A, A is the loop header.
// - Multiple backedges may point to the same tail (i.e. loop header) but have different heads.
//   - Consider: `continue` statements.
// - A backedge may point to a loop header outside its own loop.
//   - Consider: labelled `continue` statements.
// - A loop may be inside another loop; therefore, nodes may be part of multiple loops.
pub(crate) fn find_loops(
  cfg_parents: &AHashMap<u32, Bitmap>,
  backedges: &AHashMap<u32, u32>, // Map from A -> B where B -> A is a backedge.
) -> AHashMap<u32, Bitmap> {
  // Map from header -> nodes (including the header).
  let mut loops = AHashMap::<u32, Bitmap>::new();
  for (&header, &b) in backedges {
    let l = loops.entry(header).or_default();
    l.add(header);
    l.add(b);
    let mut queue = [b].into_iter().collect::<VecDeque<_>>();
    let mut seen = Bitmap::of(&[header, b]);
    while let Some(n) = queue.pop_front() {
      // We have reducible CFGs, so all parents must be part of the loop; they can't magically enter from outside the loop.
      for p in cfg_parents[&n].iter() {
        if seen.contains(p) {
          continue;
        };
        seen.add(p);
        l.add(p);
        queue.push_back(p);
      };
    };
  };
  loops
}
