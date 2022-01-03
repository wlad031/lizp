# Lizp

[![build](https://img.shields.io/github/workflow/status/wlad031/lizp/Scala%20CI?label=CI&logo=GitHub&style=flat-square)](https://github.com/wlad031/lizp/actions)
[![codecov](https://img.shields.io/codecov/c/github/wlad031/lizp?label=cov&logo=Codecov&style=flat-square)](https://codecov.io/gh/wlad031/lizp)

Lizp is my experimental dialect of [Lisp language](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

Lizp is also an interpreter for this language written in Scala.

### Examples

#### Fibonacci numbers
```
(include "std/std.lz")

(def fib [n]
  (def iter [i a b]
    (if (< i n) 
        (iter (+ i 1) b (+ a b))
        a))
  (if (= n 1) 0 (iter 0 0 1)))

(fib 1)
(fib 2)
(fib 10)
(fib 100)
```
Outputs:
```
> LUnit       // function definition returns Unit type
> LNum(0)
> LNum(1)
> LNum(55)
> LNum(354224848179261915075)
```

### Language syntax

Bracket pairs `()`, `[]` and `{}` are interchangeable, but all expressions must keep bracket-balance.

#### Constants
```
(val hello "Hello")
```

#### Functions

##### Definition
```
(def greeting [name] (println hello name))
```

#### Calling
```
(greeting "John")
```

#### Conditions
```
(if (> 1 0) (println true) (println false))
```

#### Lambda (anonymous functions)
```
(def foo [f a b] (f a b))

(foo (lambda [x y] (+ x y)) 10 20)
(foo (lambda [x y] (* x y)) 10 20)
(foo (lambda [x y] (/ y x)) 10 20)
```

### Including
```
(include "path/to/another/script.lz")
```

All standard definitions are located in `std/std.lz` (including `+`, `println` and other simple functions!), so you should put `(include "std/std.lz")` into almost any program.


### Features

- [x] optimization for tail-recursive functions (for very simple ones, as, for example, `iter` from the example above)

