(() => {
  var x;
  // Redeclaration is disallowed, even if previous declaration was `var` and not `let`/`const`.
  // let x;
  {
    // However, within a nested block is fine.
    let x;
  };

  let y;
  // This is disallowed, despite being `var`.
  // var y;

  var z;
  // However, this is fine.
  var z;
})();



try {
  (() => {
    // Use before declaration of `let`/`const` is strictly disallowed, but can be caught at runtime.
    console.log(y);
    let y;
  })();
} catch (err) {
  console.error(err.message);
}



try {
  (() => {
    // This is because sometimes, it's only possible to know at runtime.
    if (Math.random() >= 0.5) {
      console.log(y);
    }
    let y;
  })();
} catch (err) {
  console.error(err.message);
}



try {
  var z = 5;
  (() => {
    // Note that a declaration applies to the whole scope, not just after its declaration.
    // This still causes a use-before-declaration error, and doesn't simply use `z` in the outer scope.
    // This means symbol resolvers need two passes: one to find the declarations, and then one to know what usages are referring to. It cannot be done in one pass.
    console.log(z);
    let z = 6;
  })();
} catch (err) {
  console.error(err.message);
}
