use ahash::{HashMap, HashMapExt, HashSet, HashSetExt};

use std::{collections::VecDeque, ops::{Deref, DerefMut}};

use crate::{graph::graph::Graph, il::{inst::{Inst, InstTyp}, source_to_inst::DUMMY_LABEL}};

/// Wrapper over a Graph<u32> that provides owned types and better method names,
/// as well as domain-specific methods.
pub struct CfgGraph(Graph<u32>);

impl CfgGraph {
  pub fn labels(&self) -> impl Iterator<Item=u32> + '_ {
    self.0.nodes().cloned()
  }

  pub fn parents(&self, bblock: u32) -> impl Iterator<Item=u32> + '_ {
    self.0.parents(&bblock).cloned()
  }

  pub fn children(&self, bblock: u32) -> impl Iterator<Item=u32> + '_ {
    self.0.children(&bblock).cloned()
  }

  pub fn connect(&mut self, parent: u32, child: u32) {
    self.0.connect(&parent, &child);
  }

  pub fn disconnect(&mut self, parent: u32, child: u32) {
    self.0.disconnect(&parent, &child);
  }

  pub fn delete_many(&mut self, bblocks: impl IntoIterator<Item=u32>) {
    for bblock in bblocks {
      self.0.contract(&bblock);
    }
  }

  /// Remove a disconnected bblock from the graph.
  pub fn pop(&mut self, bblock: u32) {
    self.0.pop(&bblock);
  }

  /**
   * WARNING: It is dangerous to remove empty bblocks, as they can have significance despite being empty:
   * - Phi insts still need to distinguish between different branches for different values. This can happen quite often with const propagation, where an empty block merely determines which const value in a Phi is picked.
   * When is it safe to remove a bblock (and move insts and fix up parent-child relationships and Phi insts)?
   * - When a bblock has exactly one parent and that parent has exactly one child.
   * - When an empty bblock has no children.
   * - When an empty bblock has no children with Phi insts.
   */
  pub fn find_unreachable(&self) -> impl Iterator<Item=u32> + '_ {
    let mut seen = HashSet::from_iter([0]);
    let mut to_visit = VecDeque::from([0]);
    while let Some(n) = to_visit.pop_front() {
      for &c in self.0.children(&n) {
        if !seen.contains(&c) {
          seen.insert(c);
          to_visit.push_back(c);
        };
      }
    }
    // Find unreachable bblocks.
    self.0.nodes().filter(move |n| !seen.contains(n)).cloned()
  }
}

/// Wrapper over a HashMap that provides owned types and better method names,
/// as well as domain-specific methods.
pub struct CfgBBlocks(HashMap<u32, Vec<Inst>>);

impl CfgBBlocks {
  pub fn get(&self, label: u32) -> &Vec<Inst> {
    self.0.get(&label).unwrap()
  }

  pub fn get_mut(&mut self, label: u32) -> &mut Vec<Inst> {
    self.0.get_mut(&label).unwrap()
  }

  pub fn add(&mut self, label: u32, bblock: Vec<Inst>) {
    assert!(self.0.insert(label, bblock).is_none());
  }

  pub fn remove(&mut self, label: u32) -> Vec<Inst> {
    self.0.remove(&label).unwrap()
  }

  pub fn all(&self) -> impl Iterator<Item=(u32, &Vec<Inst>)> {
    self.0.iter().map(|(k, v)| (*k, v))
  }

  pub fn all_mut(&mut self) -> impl Iterator<Item=(u32, &mut Vec<Inst>)> {
    self.0.iter_mut().map(|(k, v)| (*k, v))
  }
}

/// Control flow graph. Contains the bblock graph and the bblocks themselves.
pub struct Cfg {
  // We store these as different fields because we often want to mutate one while holding a reference to the other. If we only provide &mut self methods, we'd have to borrow both mutably at the same time.
  pub graph: CfgGraph,
  pub bblocks: CfgBBlocks,
}

impl Cfg {
  pub fn from_bblocks(
    bblocks: HashMap<u32, Vec<Inst>>,
    // We consume this because all subsequent analysis operations should use a well-defined order (e.g. reverse postorder) for safety/correctness, and not this rather arbitrary ordering.
    bblock_order: Vec<u32>,
  ) -> Self {
    let mut graph = Graph::new();
    for i in 0..bblocks.len() {
      let parent = bblock_order[i];
      if let Some(Inst { t: InstTyp::Goto | InstTyp::CondGoto, labels, .. }) = &bblocks[&parent].last() {
        for mut label in labels.iter().cloned() {
          // We use DUMMY_LABEL during source_to_inst for one branch of a CondGoto to indicate fallthrough.
          if label == DUMMY_LABEL {
            label = bblock_order[i + 1];
          };
          graph.connect(&parent, &label);
        }
      } else if i == bblocks.len() - 1 {
        // Last bblock, don't connect to anything.
      } else {
        // Implicit fallthrough.
        graph.connect(&parent, &bblock_order[i + 1]);
      };
    }
    Self {
      graph: CfgGraph(graph),
      bblocks: CfgBBlocks(bblocks),
    }
  }
}
