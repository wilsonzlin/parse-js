use ahash::AHashMap;
use croaring::Bitmap;
use serde::Serialize;

use crate::inst::Inst;

#[derive(Serialize)]
pub struct OptimizerDebugStep {
  pub name: String,
  pub bblock_order: Vec<u32>,
  pub bblocks: AHashMap<u32, Vec<Inst>>,
  pub cfg_children: AHashMap<u32, Vec<u32>>,
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

  pub fn add_step(&mut self, name: impl AsRef<str>, bblock_order: &[u32], bblocks: &AHashMap<u32, Vec<Inst>>, cfg_children: &AHashMap<u32, Bitmap>) {
    self.steps.push(OptimizerDebugStep {
      name: name.as_ref().to_string(),
      bblock_order: bblock_order.to_vec(),
      bblocks: bblocks.clone(),
      cfg_children: cfg_children.iter().map(|(k, v)| (*k, v.to_vec())).collect(),
    });
  }
}
