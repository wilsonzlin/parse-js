use clap::Parser;
use parse_js::parse;
use parse_js::parse::toplevel::TopLevelMode;
use parse_js::session::Session;
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
  let session = Session::new();
  let parsed = parse(&session, &source, args.mode).expect("parse");
  serde_json::to_writer(stdout(), &parsed).expect("write to stdout");
}
