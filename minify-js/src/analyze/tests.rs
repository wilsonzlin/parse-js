use ahash::{AHashMap, AHashSet};
use parse_js::{parse, visit::Visitor};
use symbol_js::{
  compute_symbols,
  symbol::{Scope, ScopeType, Symbol},
  TopLevelMode,
};

use super::VarVisitor;

fn parse_and_visit(source: &[u8]) -> (Scope, VarVisitor) {
  let mut parsed = parse(source).unwrap();
  let top_level_scope = compute_symbols(&mut parsed, TopLevelMode::Global);
  let mut var_visitor = VarVisitor::default();
  var_visitor.visit(&parsed);
  (top_level_scope, var_visitor)
}

struct T {
  typ: ScopeType,
  // Record the symbol ID of .0 into the returned map at entry with key .1.
  syms: Vec<(&'static str, &'static str)>,
  children: Vec<T>,
}

fn test_scope_tree(out: &mut AHashMap<&'static str, Symbol>, s: &Scope, m: &T) {
  let sd = s.data();
  assert_eq!(sd.typ(), m.typ);
  assert_eq!(sd.symbol_count(), m.syms.len());
  for (s, k) in m.syms.iter() {
    let Some(sym) = sd.get_symbol(s.to_string()) else {
      panic!("did not find the declaration for {s}")
    };
    assert!(out.insert(k, sym).is_none());
  }
  assert_eq!(sd.children().len(), m.children.len());
  for (i, c) in sd.children().iter().enumerate() {
    test_scope_tree(out, c, &m.children[i]);
  }
}

#[test]
fn test_var_visitor() {
  let (s, v) = parse_and_visit(
    br#"
      (() => {
        let a, b;
        z;
        (() => {
          let c;
          y, b, c;
          (() => {
            let b;
            z, y, x, a;
            {
              let d;
              b, w;
              {
                let e;
                z, w, c, b, a;
              }
            }
          })();
        })();
      })();
    "#,
  );

  // Verify the entire scope tree from the top.
  let mut syms = AHashMap::new();
  #[rustfmt::skip]
  test_scope_tree(&mut syms, &s, &T {
    typ: ScopeType::Global,
    syms: vec![],
    children: vec![
      T {
        typ: ScopeType::ArrowFunction,
        syms: vec![("a", "a"), ("b", "b1")],
        children: vec![
          T {
            typ: ScopeType::ArrowFunction,
            syms: vec![("c", "c")],
            children: vec![
              T {
                typ: ScopeType::ArrowFunction,
                syms: vec![("b", "b2")],
                children: vec![
                  T {
                    typ: ScopeType::Block,
                    syms: vec![("d", "d")],
                    children: vec![
                      T {
                        typ: ScopeType::Block,
                        syms: vec![("e", "e")],
                        children: vec![],
                      },
                    ],
                  },
                ],
              },
            ],
          },
        ],
      },
    ],
  });

  // Verify the visit.
  assert_eq!(
    v.declared,
    AHashSet::from([
      syms["a"],
      syms["b1"],
      syms["b2"],
      syms["c"],
      syms["d"],
      syms["e"],
    ]),
  );

  assert_eq!(
    v.foreign,
    AHashSet::from([
      syms["a"],
      syms["b1"],
      syms["c"],
    ]),
  );

  assert_eq!(
    v.use_before_decl.keys().cloned().collect::<AHashSet<_>>(),
    AHashSet::from([
    ]),
  );

  assert_eq!(
    v.unknown,
    AHashSet::from([
      "w".to_string(),
      "y".to_string(),
      "x".to_string(),
      "z".to_string(),
    ]),
  );
}

#[test]
fn test_var_visitor_with_globals() {
  let source = br#"
    let globalVar;
    anotherGlobalVar, z;
    {
      a, b, z;
      let a;
      {
        a, b, c, globalVar;
        let b, c;
        {
          a, b, globalVar;
          let b, d;
          d;
        }
      }
    }
  "#;
  let (s, v) = parse_and_visit(source);

  // Verify the entire scope tree from the top.
  let mut syms = AHashMap::new();
  #[rustfmt::skip]
  test_scope_tree(&mut syms, &s, &T {
    typ: ScopeType::Global,
    syms: vec![],
    children: vec![
      T {
        typ: ScopeType::Block,
        syms: vec![("a", "a")],
        children: vec![
          T {
            typ: ScopeType::Block,
            syms: vec![("b", "b1"), ("c", "c")],
            children: vec![
              T {
                typ: ScopeType::Block,
                syms: vec![("b", "b2"), ("d", "d")],
                children: vec![],
              },
            ],
          },
        ],
      },
    ],
  });

  // Verify the visit.
  assert_eq!(
    v.declared,
    AHashSet::from([
      syms["a"],
      syms["b1"],
      syms["b2"],
      syms["c"],
      syms["d"],
    ]),
  );

  assert_eq!(
    v.foreign,
    AHashSet::from([
    ]),
  );

  assert_eq!(
    v.use_before_decl.keys().cloned().collect::<AHashSet<_>>(),
    AHashSet::from([
      syms["a"],
      syms["b1"],
      syms["b2"],
      syms["c"],
    ]),
  );

  assert_eq!(
    v.unknown,
    AHashSet::from([
      "anotherGlobalVar".to_string(),
      "b".to_string(),
      "globalVar".to_string(),
      "z".to_string(),
    ]),
  );
}
