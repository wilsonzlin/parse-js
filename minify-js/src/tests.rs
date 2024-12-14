
use symbol_js::TopLevelMode;

use crate::minify;

#[test]
fn test_minify() {
let mut out = Vec::new();
minify(
  TopLevelMode::Global,
  br##"

  let myvar = 1;

  "##,
  &mut out
).unwrap();
}
