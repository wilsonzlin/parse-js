use clap::Parser;
use parse_js::parse;
use parse_js::parse::toplevel::TopLevelMode;
use parse_js::serialize::serialize_ast;
use std::io::stdin;
use std::io::stdout;
use std::io::Read;

#[derive(Parser, Debug)]
#[command(author, version)]
struct Cli {
   #[arg(short, long)]
   mode: TopLevelMode,
}

fn main() {
    let args = Cli::parse();

    let mut source = Vec::new();
    stdin().read_to_end(&mut source).expect("read from stdin");
    let parsed = parse(source, args.mode).expect("parse");
    let ast = serialize_ast(
      &parsed.scope_map,
      &parsed.node_map,
      parsed.top_level_node_id,
    );
    serde_json::to_writer(stdout(), &ast).expect("write to stdout");
}
