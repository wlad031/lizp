# Lizp

[![build](https://img.shields.io/github/workflow/status/wlad031/lizp/Scala%20CI?label=CI&logo=GitHub&style=flat-square)](https://github.com/wlad031/lizp/actions)
[![codecov](https://img.shields.io/codecov/c/github/wlad031/lizp?label=cov&logo=Codecov&style=flat-square)](https://codecov.io/gh/wlad031/lizp)

Lizp is my experimental dialect of [Lisp language](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

Lizp is also an interpreter for this language written in Scala.

### Examples

#### Fibonacci numbers
```
(def (fib n)
  (def (iter i a b)
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

#### Constants
```
(val hello "Hello")
```

#### Functions

##### Definition
```
(def (greeting name) (println hello name))
```

#### Calling
```
(greeing "John")
```

#### Conditions
```
(if (> 1 0) (println true) (println false))
```

### Features

- [x] optimization for tail-recursive functions (for very simple ones, as, for example, `iter` from the example above)

