use parse_js::{error::SyntaxError, loc::Loc};

#[derive(Clone, Debug)]
pub enum MinifyError {
  Syntax(SyntaxError),
  UseBeforeDecl(Loc),
}
