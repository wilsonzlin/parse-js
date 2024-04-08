These two are equivalent. This does mean that fundamentally, classes are really just closures. However, `arguments` isn't allowed in a property initialiser.

```js
class A extends B {
  static s = 1;
  // `this` in a static initialiser refers to the class, not an instance.
  static s2 = this.s + 1;
  // Property initialisers have access to the new instance via `this`, as if they were inside the constructor.
  p = 1;
  // Property initialisers run at the start of the constructor, in order.
  p2 = this.p + 2;

  constructor(arg) {
    // Properties can also be initialised inside the constructor.
    this.p3 = this.p2 + arg + A.s;
  }

  // Arrow "methods" aren't actually methods, but regular property initialisers. They just happen to inherit `this` like any other property initialiser, so `this` works just like in a regular method. However, they are *not* on the prototype.
  ma = () => this.p;
  // Regular methods are on the prototype.
  m() {
    return this.p2;
  }
}
```

```js
function A(arg) {
  this.p = 1;
  this.p2 = this.p + 2;
  this.p3 = this.p2 + arg + A.s;
  this.ma = () => this.p;
}
A.s = 1;
A.s2 = A.s + 1;
A.prototype = Object.create(B.prototype);
A.prototype.m = function() {
  return this.p2;
};
```
