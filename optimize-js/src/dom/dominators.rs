use ahash::{HashMap, HashMapExt, HashSet};


// https://www.cs.tufts.edu/comp/150FP/archive/keith-cooper/dom14.pdf
pub fn calculate_dominated_by(idom_by: &HashMap<u32, u32>, root: u32) -> HashMap<u32, HashSet<u32>> {
  let mut dom = HashMap::<u32, HashSet<u32>>::new();
  for label in idom_by.keys().cloned() {
    let e = dom.entry(label).or_default();
    let mut n = label;
    loop {
      e.insert(n);
      if n == root {
        break;
      };
      n = idom_by[&n];
    }
  }
  dom
}
