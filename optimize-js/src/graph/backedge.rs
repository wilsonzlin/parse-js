use ahash::{HashMap, HashMapExt, HashSet, HashSetExt};

// We must use DFS, as it's only cycles within a path, not revisited nodes across the entire walk, that count as backedges.
fn inner(
  visited: &mut HashSet<u32>,
  stack: &mut Vec<u32>,
  backedges: &mut HashMap<u32, u32>, // Map from A -> B where B -> A is a backedge.
  junctions: &mut HashSet<u32>, // Nodes that have more than 1 parent and AREN'T backedges.
  cfg_children: &HashMap<u32, HashSet<u32>>,
  label: u32,
) {
  for &c in cfg_children[&label].iter() {
    if stack.contains(&c) {
      // This is a backedge.
      backedges.insert(c, label);
    } else {
      if visited.contains(&c) {
        junctions.insert(c);
      }
      visited.insert(c);
      stack.push(c);
      inner(visited, stack, backedges, junctions, cfg_children, c);
      stack.pop();
    };
  }
}

pub struct BackedgesAndJunctions {
  // A map from A -> B where B -> A is a backedge.
  pub backedges: HashMap<u32, u32>,
  pub junctions: HashSet<u32>,
}

pub fn find_backedges_and_junctions(
  cfg_children: &HashMap<u32, HashSet<u32>>,
) -> BackedgesAndJunctions {
  let mut backedges = HashMap::<u32, u32>::new();
  let mut visited = HashSet::new();
  let mut junctions = HashSet::new();
  inner(&mut visited, &mut vec![0], &mut backedges, &mut junctions, cfg_children, 0);
  BackedgesAndJunctions { backedges, junctions }
}

#[cfg(test)]
mod tests {
    use ahash::{HashMap, HashMapExt, HashSet, HashSetExt};
    use itertools::Itertools;

    use super::{find_backedges_and_junctions, BackedgesAndJunctions};

  #[test]
  fn test_find_backedges() {
    let mut cfg_children = HashMap::<u32, HashSet<u32>>::new();

    cfg_children.insert(0, HashSet::from_iter([1, 5]));
    cfg_children.insert(1, HashSet::from_iter([2, 4]));
    cfg_children.insert(2, HashSet::from_iter([3]));
    cfg_children.insert(3, HashSet::from_iter([1]));
    cfg_children.insert(4, HashSet::from_iter([5]));
    cfg_children.insert(5, HashSet::new());

    let BackedgesAndJunctions {
      backedges,
      junctions,
    } = find_backedges_and_junctions(&cfg_children);
    assert_eq!(backedges.into_iter().collect_vec(), vec![(1, 3)]);
    assert_eq!(junctions.into_iter().collect_vec(), vec![5]);
  }
}
