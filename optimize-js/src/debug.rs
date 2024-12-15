use ahash::{HashMap, HashSet};
use serde::Serialize;

use crate::{inst::Inst, postorder::calculate_postorder};

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub struct OptimizerDebugStep {
  pub name: String,
  pub bblock_order: Vec<u32>,
  pub bblocks: HashMap<u32, Vec<Inst>>,
  pub cfg_children: HashMap<u32, HashSet<u32>>,
}

#[derive(Serialize)]
pub struct OptimizerDebug {
  steps: Vec<OptimizerDebugStep>,
}

impl OptimizerDebug {
  pub fn new() -> Self {
    Self {
      steps: Vec::new(),
    }
  }

  pub fn add_step(&mut self, name: impl AsRef<str>, bblocks: &HashMap<u32, Vec<Inst>>, cfg_children: &HashMap<u32, HashSet<u32>>) {
    self.steps.push(OptimizerDebugStep {
      name: name.as_ref().to_string(),
      // We always recalculate as some steps may prune or add bblocks.
      bblock_order: calculate_postorder(&cfg_children, 0).0,
      bblocks: bblocks.clone(),
      cfg_children: cfg_children.iter().map(|(k, v)| (*k, v.clone())).collect(),
    });
  }
}
