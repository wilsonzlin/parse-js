use optimize_js::compile_js_statements;
use optimize_js::symbol::var_visitor::VarAnalysis;
use optimize_js::util::debug::OptimizerDebug;
use parse_js::ast::Node;
use parse_js::ast::Syntax;
use parse_js::parse;
use serde::Serialize;
use symbol_js::compute_symbols;
use symbol_js::TopLevelMode;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[wasm_bindgen]
pub fn set_panic_hook() {
  // When the `console_error_panic_hook` feature is enabled, we can call the
  // `set_panic_hook` function at least once during initialization, and then
  // we will get better error messages if our code ever panics.
  //
  // For more details see
  // https://github.com/rustwasm/console_error_panic_hook#readme
  #[cfg(feature = "console_error_panic_hook")]
  console_error_panic_hook::set_once();
}

#[derive(Serialize)]
pub struct BuiltJs {
  pub ast: Node,
  pub debug: OptimizerDebug,
}

#[wasm_bindgen]
pub fn build_js(source: &str, is_global: bool) -> JsValue {
  let top_level_mode = if is_global {
    TopLevelMode::Global
  } else {
    TopLevelMode::Module
  };
  let mut top_level_node = parse(source.as_bytes()).expect("parse input");
  compute_symbols(&mut top_level_node, top_level_mode);

  let VarAnalysis {
    declared,
    foreign,
    unknown,
    use_before_decl,
  } = VarAnalysis::analyze(&top_level_node);
  if let Some((_, loc)) = use_before_decl.iter().next() {
    panic!("Use before declaration at {:?}", loc);
  };
  let Syntax::TopLevel { body } = top_level_node.stx.as_ref() else {
    panic!();
  };
  let mut dbg = OptimizerDebug::new();
  let optimized = compile_js_statements(&body, Some(&mut dbg));
  let built = BuiltJs {
    ast: top_level_node,
    debug: dbg,
  };
  serde_wasm_bindgen::to_value(&built).unwrap()
}
