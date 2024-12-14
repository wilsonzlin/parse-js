(() => {
  function myFn(i) {
    // This overrides the myFn in the outer scope. However, if we add a `var myFn` here, it won't. This indicates that a function declaration name is exclusively declared in the containing scope, and not within itself.
    // var myFn;
    myFn = () => console.log("inside");
    if (i > 0) {
      console.log(i);
      myFn(i - 1);
    }
  }
  myFn(3);
  // This will log "inside".
  myFn(3);
})();

console.log();
console.log();
console.log();

(() => {
  // Even within the function declaration, the name is not in a "special" scope visible only to its body; updating the outer `myFn` does in fact change the call of `myFn` within the function. This further reinforces that the name is only declared in the containing scope.
  function myFn(i) {
    if (i > 0) {
      console.log(i);
      myFn(i - 1);
    }
  }
  const myFn2 = myFn;
  var myFn = () => console.log("replaced");
  myFn2(2);
})();

console.log();
console.log();
console.log();

(() => {
  (function myFn(i) {
    // OTOH, with a function expression, the name is visible to its body, but cannot be changed. This does nothing. (In strict mode, this would throw: "TypeError: Assignment to constant variable.")
    myFn = () => console.log("inside");
    // This also does nothing.
    myFn = 3;
    if (i > 0) {
      console.log(i);
      myFn(i - 1);
    }
  })(2);

  (function myFn(i) {
    // However, the name doesn't count as a declaration, so using a `let` here with the same name is allowed. (Normally, using `let` with an existing var name is an error, even if previously declared with `var`.)
    let myFn = () => console.log("inside");
    if (i > 0) {
      console.log(i);
      myFn(i - 1);
    }
  })(2);
})();
