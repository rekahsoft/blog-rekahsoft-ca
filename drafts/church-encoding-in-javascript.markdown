---
title: Church Encoding in JS
author: Collin J. Doering
date: Jun 19, 2015
description: Encoding various types using only variables and functions
tags: general
---

In this post I will briefly explain the [untyped lambda calculus][], then go on to explain
[church encoding][]. Following that, we will look at an implementation of church encoding using
javascript; in this article we will focus on encoding booleans as well as natural numbers, as
they are the simplest to understand. We will then provide functions to operate on our
representation as well as functions to marshal to and from our representation to javascripts.
Along the way we will also cover the topic of [currying][], as it will be used extensively in
the javascript implementation. In the future I hope to go over pairs, lists, and perhaps some
other types which are more complicated but for the time being booleans and natural numbers
should provide a good enough introduction to [church encoding][].

TLDR: Using only functions of one argument and variables (and assuming infinite space), one can
compute any computable function. Here we will implement booleans and natural numbers using only
functions and variable bindings to functions; we also implement functions to operate on those
types (Eg. and, or, xor, not, add, subtract, multiply, etc…).

<!--more-->

As I was learning church encoding and lambda calculus I implemented church encodings of various
types along with functions that operate on those types. I did this in a few of my favorite
languages; namely in [Haskell](https://www.haskell.org/), [Racket](https://www.haskell.org/),
and [Lazy Racket](http://docs.racket-lang.org/lazy/index.html), though the implementations were
to varying degrees of completion. Now many Haskell and Racket programmers are more likely to be
aware of the lambda calculus and church encoding. However, I think most javascript programmers
likely have never heard of the concept and would benefit by taking a moment to bend their
minds.

Before we get started into the 'meat' of this article, I need to explain curried functions as
they are used extensively in the javascript implementation of church encoded booleans and
natural numbers. If you already know what a curried function is, feel free to skip this
section.

Suppose you are given the challenge of writing a function that takes two arguments and returns
its first. This is easy in every language, and javascript is no exception. Now consider how
you would do it if you had the following restrictions:

1. You can only use lambda functions and variables
2. Lambda functions can only take one argument. It cannot be an object or array with some
   expected structure. For example, accepting an array and expecting the arguments to be at
   index 0 and 1 respectively is considered a violation.

At first glance this may seem like a impossible task, but no need to fret. The solution: write
a function that accepts the first argument but then returns a function which will accept the
second argument and throw it away.

  ``` {.javascript .code-term .numberLines }
  var lconst = function (x) {
    var ret = function {y) {
      return x;
    };
    return ret;
  };
  ```

On line 2-5 we use a variable `ret` to temporarily store a function to accept the 'second'
argument; we then return this function. The function itself closes over the argument to the
outer function and returns it. I could have written `return function (y) { return x; };`
instead of lines 1-3 but prefer the added readability of newlines; but to do this and avoid
javascripts automatic semicolon insertion we have to temporarily store the function in a
variable. This is because semicolons are inserted automatically after return statements in
javascript. For more details on javascripts automatic semicolon insertion see section 7.9.1 of the
[ECMAScript 2015 language specification](http://www.ecma-international.org/publications/standards/Ecma-262.htm).

Now lets see our `lconst` function in action:

  ``` {.javascript .code-term}
  lconst(1)(2);               // 1
  lconst("Alonzo")("Church"); // "Alonzo"
  var alwaysZero = lconst(0); // undefined
  alwaysZero(1);              // 0
  alwaysZero("Alonzo");       // 0
  ```


TODO: Finish explanation about curried functions




Now that we have gone over curried functions, we need to take a gander at the lambda calculus
before we get started with church encoding and its implementation in javascript. Here I will
describe it in terms of javascript, but I highly recommend taking the time to read the wiki
page on the subject as it does a better job describing it then I can. Particularly, my
description of the reduction rules are quite rough and lack rigor, but a full description of
the lambda calculus is beyond the scope of this article.

The lambda calculus' syntax can be given by three simple rules. A lambda term is any
one of the following:

Variable
  ~ A single letter symbol (Eg. `x`)

Lambda Abstraction
  ~ if `t` is a lambda term and `x` is a variable, then `λx.t` is a lambda term

Application
  ~ if `t` and `s` are lambda terms, then `(t s)` is a lambda term

Now that we can specify a lambda term we now have to talk about how they can be reduced. There
are three types of reduction.

α-conversion (alpha)

  ~ Also called alpha-renaming, allows bound variable names to be changed. For example,
    `λx.λy.x` can be alpha converted to `λa.λb.a` or `λy.λx.y` among others. By changing the
    bound variables, the original meaning of the lambda expression is retained. That is, both
    `λx.λy.x` and `λa.λb.a` both can be thought of as curried functions of two arguments that
    return their first argument.

β-reduction (beta)
  ~ TODO

η-conversion (eta)
  ~ TODO

When a two lambda terms can be reduced to the same expression by α-conversion we say they are
α-equivalent. β-equivalence and η-equivalence can be defined similarly.



TODO: Talk about lambda term reduction α-conversion (alpha), β-reduction (beta), η-conversion (eta)





Some lambda terms are used commonly, and have standard names. Here are some examples

- The `id` function (identity), that is a function that returns its argument. In the lambda
  calculus its given as `λx.x` whose corresponding javascript is as follows.

  ``` {.javascript .code-term }
  function id (x) {
    return x;
  };
  ```

- The `const` function, which is a curried function of two arguments that returns its first
  argument. It is given in the lambda calculus as `λx.λy.x` and can be written in javascript as
  follows. Note however, that the function given in javascript can not have the name `const` as
  its a reserved word in javascript; instead we use `lconst`.

  TODO reference lconst js definition above

- Similar to the `const` function but with no standard name, what I'll call `constid` is a
  curried function of two arguments that returns its second argument (unlike `const` which
  returns the first one). It is given by `λx.λy.y` in the lambda calculus and can be written in
  javascript as follows.

  ``` {.javascript .code-term}
  var constid = function (x) {
    var ret = function (y) {
      return y;
    };
    return ret;
  };
  ```

  Notice that the `constid` function can be obtained by applying `const` to `id` in the lambda
  calculus (`const id`). Similarly in javascript, the `lconst` function can be applied to the
  `id` to obtain `constid`.

  ``` {.javascript .code-term}
  var constid = lconst(id);
  ```

Now by this point you are likely scratching your head a little. How in the world can just
functions and variable bindings alone allow us to represent booleans, and nature numbers; let alone
have the ability to express any turing computable function.


    ((λx.λy.x) id)
    ≡ (λy.id)
    ≡ (λy.λa.a)
    ≡ constid









``` {.javascript .code-term .numberLines}
/**
 * (C) Copyright Collin J. Doering 2015
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * File: church.js
 * Author: Collin J. Doering <collin.doering@rekahsoft.ca>
 * Date: Feb  2, 2015
 */

var church = (function () {
    var spec,
        t = function (x) {
            var ret = function (y) {
                return x;
            };
            return ret;
        },
        f = function (x) {
            var ret = function (y) {
                return y;
            };
            return ret;
        },
        church_zero = function (f) {
            var ret = function (x) {
                return x;
            };
            return ret;
        },
        nil;

    // ------------------------------
    // Church Boolean implementation
    // ------------------------------

    function make_bool (b) {
        // b ? return t : return f
        if (b) {
            return t;
        } else {
            return f;
        }
    }

    function unchurch_bool (b) {
        return b(true)(false);
    }

    function and (x) {
        var ret = function (y) {
            return x(y)(x);
        };
        return ret;
    }

    function or (x) {
        var ret = function (y) {
            return x(x)(y);
        };
        return ret;
    }

    function not (x) {
        var ret = function (y) {
            var aret = function (z) {
                return x(z)(y);
            };
            return aret;
        };
        return ret;
    }

    function xor (x) {
        var ret = function (y) {
            return x(not(y))(y);
        };
        return ret;
    }

    function church_if (p) {
        var ret = function (a) {
            var aret = function (b) {
                return p(a)(b);
            };
            return aret;
        };
        return ret;
    }

    // -----------------------
    // Church natural numbers
    // -----------------------

    function succ (n) {
        var ret = function (f) {
            var aret = function (x) {
                return f(n(f)(x));
            };
            return aret;
        };
        return ret;
    }

    function add (n) {
        var ret = function (m) {
            var aret = function (f) {
                var bret = function (x) {
                    return n(f)(m(f)(x));
                };
                return bret;
            };
            return aret;
        };
        return ret;
    }

    function mult (n) {
        var ret = function (m) {
            var aret = function (f) {
                var bret = function (x) {
                    return n(m(f))(x);
                };
                return bret;
            };
            return aret;
        };
        return ret;
    }

    function expt (n) {
        var ret = function (m) {
            var aret = function (f) {
                var bret = function (x) {
                    return (m(n))(f)(x);
                };
                return bret;
            };
            return aret;
        };
        return ret;
    }

    function isZero (n) {
        var ret = n(function (x) {
            return f;
        })(t);
        return ret;
    }

    function make_nat (n) {
        var i, ret = church_zero.bind({});
        for (i = 0; i < n; i += 1) {
            ret = succ(ret);
        }
        return ret;
    }

    function unchurch_nat (n) {
        var ret = function (i) {
            var aret = function (m) {
                i += 1;
                return i;
            };
            return aret;
        }, i = 0;
        return n(ret(i))(0);
    }

    // -------------
    // Church Pairs
    // -------------

    function make_pair (a) {
        var ret = function (b) {
            var aret = function (f) {
                return f(a)(b);
            };
            return aret;
        };
        return ret;
    }

    function fst (p) {
        return p(t);
    }

    function snd (p) {
        return p(f);
    }

    function unchurch_pair (p) {
        return [fst(p), snd(p)];
    }

    // -------------
    // Church Lists
    // -------------

    function make_list () {}

    function unchurch_list (xs) {}

    nil = make_pair(t)(t);
    isNil = fst;

    function cons (h) {
        var ret = function (t) {
            return make_pair(f)(make_pair(h)(t));
        };
        return ret;
    }

    function head (l) {
        return fst(snd(l));
    }

    function tail (l) {
        return snd(snd(l));
    }

    // -----------------
    // The Y Combinator
    // -----------------
    // * This doesn't work as javascript is strictly evaluated
    // -----------------
    
    function fix (g) {
        var f = function (x) {
            return g(x(x));
        };
        return f(f);
    }

    // Setup specification object
    spec = {
        "if": church_if,
        fix: fix,
        bool: {
            make: make_bool,
            toNative: unchurch_bool,
            t: t,
            f: f,
            not: not,
            and: and,
            or: or,
            xor: xor
        },
        nat: {
            make: make_nat,
            toNative: unchurch_nat,
            zero: church_zero,
            succ: succ,
            add: add,
            mult: mult,
            expt: expt,
            isZero: isZero
        },
        pair: {
            make: make_pair,
            toNative: unchurch_pair,
            fst: fst,
            snd: snd
        },
        list: {
            make: make_list,
            toNative: unchurch_list,
            nil: nil,
            isNil: isNil,
            cons: cons,
            head: head,
            tail: tail
        }
    };
    return spec;
})();

// -------------------------

function unchurch_church_nat_test (lim) {
    var i,
        lim = lim || 12;
    for (i = 0; i <= lim; i += 1) {
        if (i !== church.nat.toNative(church.nat.make(i))) {
            console.log('Failed church.nat.toNative(church.nat.make(' + i + '))');
            return false;
        }
    }
    console.log('Created church nats from 1 to ' + lim + ' successfully.');
    return true;
}

function uncurry (f) {
    var ret = function (x, y) {
        return f(x)(y);
    };
    return ret;
}

function matrix_test (f, g, n, tp) {
    var i, j,
        t = tp || 100,
        name = n || "unnamed";
    for (i = 0; i <= t; i += 1) {
        for (j = 0; j <= t; j += 1) {
            if (f(i,j) !== g(i,j)) {
                console.log('Failed matrix test for ' + name + ' with inputs ' + i + ' and ' + j + '.');
                return false;
            }
        }
    }
    console.log('Successfully completed test for ' + name + '.');
    return true;
}

unchurch_church_nat_test(100);

matrix_test(function (a, b) {
    return a + b;
}, function (a, b) {
    return church.nat.toNative(church.nat.add(church.nat.make(a))(church.nat.make(b)));
}, "Church addition");

// matrix_test(function (a, b) {
//     return a - b;
// }, function (a, b) {
//     return church.nat.toNative(minus(church.nat.make(a))(church.nat.make(b)));
// });

matrix_test(function (a, b) {
    return a * b;
}, function (a, b) {
    return church.nat.toNative(church.nat.mult(church.nat.make(a))(church.nat.make(b)));
}, "Church multiplication");

// very slow for some reason? (not that this implementation will ever be fast)
matrix_test(function (a, b) {
    return Math.pow(a, b);
}, function (a, b) {
    return church.nat.toNative(church.nat.expt(church.nat.make(a))(church.nat.make(b)));
}, "Church exponentiation", 7);


// -------------------------------------------------------
// Factorial function written similar to how the y-combinator works
function factorial (x) {
    var g = function (h) {
        var f = function (n) {
            if (n < 2) {
                return 1;
            } else {
                return n * h(h)(n - 1);
            }
        };
        return f;
    };
    return g(g)(x);
}

var i;
for (i = 0; i < 10; i += 1) {
    console.log(factorial(i));
}
```

<!--
``` {.javascript .code-term .numberLines include="../files/source/church.js"}
```
Find a way to include source files without having to copy-paste them into the article.
See: http://stackoverflow.com/questions/21584396/pandoc-include-files-filter-in-haskell
-->

[untyped lambda calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[church encoding]: https://en.wikipedia.org/wiki/Church_encoding
[currying]: https://en.wikipedia.org/wiki/Currying
