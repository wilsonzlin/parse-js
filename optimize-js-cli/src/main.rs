use std::{fs, path::PathBuf};

use clap::{builder::PossibleValuesParser, Parser};
use optimize_js::{compile_js_statements, debug::OptimizerDebug, var_visitor::VarVisitor};
use parse_js::{ast::Syntax, parse};
use symbol_js::{compute_symbols, TopLevelMode};
use parse_js::visit::Visitor;
use clap::builder::TypedValueParser;

#[derive(Parser)]
#[command(version)]
struct Args {
  /// Path to JS file.
  #[arg()]
  input: PathBuf,

  /// Path to write debug output to.
  #[arg(long)]
  output: PathBuf,

  /// Whether file is a module or global script.
  #[arg(
    short,
    long,
    value_parser = PossibleValuesParser::new(["global", "module"])
        .map(|s| match s.as_str() {
          "global" => TopLevelMode::Global,
          "module" => TopLevelMode::Module,
          _ => unreachable!(),
        }),
  )]
  mode: TopLevelMode,
}

fn main() {
  let args = Args::parse();
  let source = fs::read(args.input).expect("read input file");
  let mut top_level_node = parse(&source).expect("parse input");
  compute_symbols(&mut top_level_node, args.mode);

  let mut var_visitor = VarVisitor::default();
  var_visitor.visit(&top_level_node);
  let VarVisitor {
    declared,
    foreign,
    unknown,
    use_before_decl,
  } = var_visitor;
  if let Some((_, loc)) = use_before_decl.iter().next() {
    panic!("Use before declaration at {:?}", loc);
  };
  let Syntax::TopLevel { body } = top_level_node.stx.as_ref() else {
    panic!();
  };
  let mut dbg = OptimizerDebug::new();
  let optimized = compile_js_statements(&body, Some(&mut dbg));
  fs::write(args.output, serde_json::to_string(&optimized).unwrap()).expect("write output file");
}
