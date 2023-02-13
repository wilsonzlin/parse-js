AST design rationale:

1. We want a type for our AST nodes:

```rust
struct Node {
  loc: Loc,
  stx: Box<Stx>, // We must store in a Box, because Stx will reference Node.
}
enum Stx {
  AddExpr { left: Node, right: Node, fast: bool },
}
```

2. We want to avoid `malloc` and use a bump arena allocator:

```rust
struct Node<'bump> {
  loc: Loc,
  stx: Stx<'bump>
}
enum Stx {
  AddExpr { left: &'bump Node<'bump>, right: &'bump Node<'bump>, fast: bool },
}
```

3. We want to be able to mutate the tree:

```rust
struct Node<'bump> {
  loc: Loc,
  stx: Stx<'bump>
}
enum Stx {
  AddExpr { left: &'mut bump Node<'bump>, right: &'mut bump Node<'bump>, fast: bool },
}
```

4. We want to be able to move subtrees around while mutating, as many transforms will require this.

In Rust, there are three main options:
- Make an entire Node cloneable. This is impossible now as we use mut references which aren't cloneable. Even if they were, cloning an entire subtree just to move it (we don't actually want a copy) is very wasteful and slow.
- Use lightweight references to Node values, for cheap cloning. This would be possible if we used something like `RefCell`, `Box`, `Rc`, or non-mut references returned from the arena, but these options either incur significant runtime overhead or don't allow subtree mutation.
- Use move semantics. For owned values, this is simple, as Rust tracks field-level moves and transparently handles them. However, we use references to Node values, so it's trickier; even with mut references, we can't move a field out of the underlying struct. We must instead use core::mem::{replace,swap,take} functions and leave the original value in a dummy/invalid state. This means the original value will still exist and be a valid reference (i.e. they'll still exist in the arena), but we must never use them.

Just as an aside, for simple mutations on a field where we don't want to utilise some other part of the tree, it's straightforward:

```rust
// These two mutations are easy, because their data is fully derived from outside the tree.
add_node.fast = true;
add_node.right = Node::new(loc, Stx::Literal(2));
```

However, for more common mutations where we're trying to transform and shuffle things around, we must use the `take` method:

```rust
// A very common pattern is to rebuild the node, often because we're changing the type.
let new_node = Node::new(loc, Stx::SubExpr {
  left: add_node.stx.left.take(),
  right: add_node.stx.right.take(),
});
// `add_node` now has fields that have been taken and left in an invalid state, so we must not use it anymore.

// Some other common patterns:
*add_node.stx = Stx::SubExpr { left: add_node.stx.left.take(), right: Stx::Literal(2) };
let (left, right) = (add_node.stx.left.take(), add_node.stx.right.take());
add_node.stx.left = right;
add_node.stx.right = left;
```

5. Can we clone a subtree?

This is not possible via the Clone trait, since Node references in the tree are mutable, and even then it would still require accessing the arena allocator to create the clones. This isn't available by default, as it's very rare to actually want to deep clone a subtree; it's expensive but doesn't transform anything. Usually transforms are about moves, and some manual partial cloning usually arises from more complex traversal logic while transforming. To clone anyway, manually do so using the visitor pattern. Note that shallow clones are also not possible as Node references are mutable and aren't copyable.

6. Summary.

- RefCell has a significant performance penalty, but we still want to be able to mutate the tree, so we keep mut references for Node values (which must be unique as per Rust rules).
- `malloc` has a significant performance penalty, so we use an arena allocator.
- For these reasons, we don't have cheaply copyable Node references, but this is OK, as we usually don't share/store Node references elsewhere and don't want to deep clone subtrees (transforms are almost always moves).
