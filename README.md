# Lizp

[![build](https://img.shields.io/github/workflow/status/wlad031/lizp/Scala%20CI?label=CI&logo=GitHub&style=flat-square)](https://github.com/wlad031/lizp/actions)
[![codecov](https://img.shields.io/codecov/c/github/wlad031/lizp?label=cov&logo=Codecov&style=flat-square)](https://codecov.io/gh/wlad031/lizp)

Lizp is my experimental dialect of [Lisp language](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

Lizp is also an interpreter for this language written in Scala.

### Examples

#### Fibonacci numbers
```
(include "std/std.lz")

(defn 'fib ['n]
  (defn 'iter ['i 'a 'b]
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
> Nil       // function definition returns Nil type
> 0
> 1
> 55
> 354224848179261915075
```

### Language syntax

Bracket pairs `()`, `[]` and `{}` are interchangeable, but all expressions must keep bracket-balance.

#### Constants
```
(def 'hello "Hello")
```

#### Functions

##### Definition
```
(defn 'greeting ['name] (println hello name))
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
(defn 'foo ['f 'a 'b] (f a b))

(foo (fn ['x 'y] (+ x y)) 10 20)
```

### Including
```
(include "path/to/another/script.lz")
```

All standard definitions are located in `std/std.lz` (including macros `defn`, `defm`, etc.), so you may want put `(include "std/std.lz")` into almost any program.


### Features

- [] optimization for tail-recursive functions (for very simple ones, as, for example, `iter` from the example above)

