use parse_js::{error::SyntaxError, loc::Loc};

pub enum MinifyError {
  Syntax(SyntaxError),
  UseBeforeDecl(Loc),
}
