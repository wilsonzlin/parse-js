use ahash::{AHashMap, AHashSet};
use croaring::Bitmap;

// We must use DFS, as it's only cycles within a path, not revisited nodes across the entire walk, that count as backedges.
fn inner(
  visited: &mut Bitmap,
  stack: &mut Vec<u32>,
  backedges: &mut AHashMap<u32, u32>, // Map from A -> B where B -> A is a backedge.
  junctions: &mut Bitmap, // Nodes that have more than 1 parent and AREN'T backedges.
  cfg_children: &AHashMap<u32, Bitmap>,
  label: u32,
) {
  for c in cfg_children[&label].iter() {
    if stack.contains(&c) {
      // This is a backedge.
      backedges.insert(c, label);
    } else {
      if visited.contains(c) {
        junctions.add(c);
      }
      visited.add(c);
      stack.push(c);
      inner(visited, stack, backedges, junctions, cfg_children, c);
      stack.pop();
    };
  }
}

pub(crate) struct BackedgesAndJunctions {
  // A map from A -> B where B -> A is a backedge.
  pub backedges: AHashMap<u32, u32>,
  pub junctions: Bitmap,
}

pub(crate) fn find_backedges_and_junctions(
  cfg_children: &AHashMap<u32, Bitmap>,
) -> BackedgesAndJunctions {
  let mut backedges = AHashMap::<u32, u32>::new();
  let mut visited = Bitmap::new();
  let mut junctions = Bitmap::new();
  inner(&mut visited, &mut vec![0], &mut backedges, &mut junctions, cfg_children, 0);
  BackedgesAndJunctions { backedges, junctions }
}

#[cfg(test)]
mod tests {
    use ahash::AHashMap;
    use croaring::Bitmap;
    use itertools::Itertools;

    use crate::minify::function::backedge::{find_backedges_and_junctions, BackedgesAndJunctions};

  #[test]
  fn test_find_backedges() {
    let mut cfg_children = AHashMap::<u32, Bitmap>::new();

    cfg_children.insert(0, Bitmap::of(&[1, 5]));
    cfg_children.insert(1, Bitmap::of(&[2, 4]));
    cfg_children.insert(2, Bitmap::of(&[3]));
    cfg_children.insert(3, Bitmap::of(&[1]));
    cfg_children.insert(4, Bitmap::of(&[5]));
    cfg_children.insert(5, Bitmap::new());

    let BackedgesAndJunctions {
      backedges,
      junctions,
    } = find_backedges_and_junctions(&cfg_children);
    assert_eq!(backedges.into_iter().collect_vec(), vec![(1, 3)]);
    assert_eq!(junctions.to_vec(), vec![5]);
  }
}
