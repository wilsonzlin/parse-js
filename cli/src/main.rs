use parse_js::parse;
use parse_js::parse::toplevel::TopLevelMode;
use parse_js::serialise::serialise_ast;
use std::io::Read;
use std::io::stdin;
use std::io::stdout;

fn main() {
  let mut source = Vec::new();
  stdin().read_to_end(&mut source).expect("read from stdin");
  let parsed = parse(source, TopLevelMode::Global).expect("parse");
  let ast = serialise_ast(&parsed.node_map, parsed.top_level_node_id);
  serde_json::to_writer(stdout(), &ast).expect("write to stdout");
}
