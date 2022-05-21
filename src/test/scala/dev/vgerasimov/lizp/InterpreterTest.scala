package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.Interpreter.*
import dev.vgerasimov.lizp.types.*
import dev.vgerasimov.lizp.renamings.PrefixScalaTypes.*
import dev.vgerasimov.lizp.syntax.*

object InterpreterTest:
  import java.nio.file.{ Paths }
  val underTest: Interpreter = Interpreter()
  given ctx: Context =
    Context(
      debug = Context.DebugMode.None,
      parser = Parser(),
      sourcePaths = Set(Source.resource("std.lz"), Source.directory("."))
    )

import InterpreterTest.{ given, * }

class InterpreterTest extends LizpTestSuite((in: (Expr, Scopes)) => underTest(in._1, in._2)):
  val p: String => Expr = parseUnsafe(Parser())

  given ignoreScopes: Asserter[(Expr, Scopes)] = (a, b) => assertEquals(a._1, b._1)

  // tests are filling the scopes with new definitions as they go
  var scopes: Scopes = Scopes.withNatives

  testOne("Nil -> no changes") { (Nil, scopes) -> (Nil, scopes) }
  testOne("Num -> no changes") { (Num(69), scopes) -> (Num(69), scopes) }
  testOne("Str -> no changes") { (Str("hello"), scopes) -> (Str("hello"), scopes) }
  testOne("Bool -> no changes") { (Bool(true), scopes) -> (Bool(true), scopes) }
  testOne("'Sym -> Sym") { (Sym("'foo"), scopes) -> (Sym("foo"), scopes) }
  testOne("fn -> no changes") {
    (Fn(List.sym(Sym("x")), Sym("x")), scopes) -> (Fn(List.sym(Sym("x")), Sym("x")), scopes)
  }
  testOne("evalOnDeref -> no changes") { (EvalOnDeref(Num(69)), scopes) -> (EvalOnDeref(Num(69)), scopes) }

  testOne("""[Num] -> [Num]""") { (p("(69)"), scopes) -> (Num(69), scopes) }
  testOne("""['Sym] -> [Sym]""") { (p("('foo)"), scopes) -> (List(Sym("foo")), scopes) }

  testOne("""['Sym Num Str] -> [Sym Num Str]""") {
    (p("""['x 69 "hello"]"""), scopes) -> (List(Sym("x"), Num(69), Str("hello")), scopes)
  }

  testOne("""['Sym Str 'Sym] -> [Sym Str Sym]""") {
    (p("""['x "hello" 'y]"""), scopes) -> (List(Sym("x"), Str("hello"), Sym("y")), scopes)
  }

  scopes = Scope(Sym("sixty-nine") -> Num(69)) :: scopes

  testOne("just deref -> value") { (p("sixty-nine"), scopes) -> (Num(69), scopes) }
  testOne("List(just deref) -> value") { (p("(sixty-nine)"), scopes) -> (Num(69), scopes) }

  testOne("lambda with no params -> correctly applied") {
    (p("""((fn [] 69))"""), scopes) -> (Num(69), scopes)
  }

  testOne("id lambda application (string) -> correctly applied") {
    (p("""((fn ['x] x) "hello")"""), scopes) -> (Str("hello"), scopes)
  }

  testOne("id lambda application (deref) -> correctly applied") {
    (p("((fn ['x] x) sixty-nine)"), scopes) -> (Num(69), scopes)
  }

  testOne("id lambda application (func) -> correctly applied") {
    (p("""((fn ['x] x) (fn ['x] x))"""), scopes) -> (Fn(List.sym(Sym("x")), Sym("x")), scopes)
  }

  testOne("defining id and application with num -> correctly defined and applied") {
    (p("(def 'id (fn ['x] x)) (id 69)"), scopes) ->
    (List(Nil, Num(69)), scopes)
  }

  testOne("if true then else -> then") { (p("(if true 69 420)"), scopes) -> (Num(69), scopes) }
  testOne("if false then else -> then") { (p("(if false 69 420)"), scopes) -> (Num(420), scopes) }

  testOne("def inside def -> correctly defined and applied") {
    (p("(def 'id (fn ['x] (list (def 'id2 (fn ['y] y)) (id2 x)))) (id 69)"), scopes) ->
    (List(Nil, List(Nil, Num(69))), scopes)
  }

  test("params of fn are not leaking") {
    underTest(p("(def 'id (fn ['x] x)) (id 69)"), scopes).map(_._2) match
      case Left(error)   => fail(s"Failed with error:\n$error")
      case Right(scopes) => assert(scopes.get(Sym("x")).isEmpty, "x is found in the scopes")
  }

  testOne("id macro is defined and applicable") {
    (
      p("""
        | (def 'id (macro ['a] a))
        | (id 69)
        | (id "hello world")
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Num(69), Str("hello world")), scopes)
  }

  testOne("plus alias macro is defined and applicable") {
    (
      p("""
        | (def 'plus (macro ['a 'b] (+ a b)))
        | (plus 60 9)
        | (plus "hello " "world")
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Num(69), Str("hello world")), scopes)
  }

  testOne("defn defined as a macro -> functions are applicable 1") {
    (
      p("""
        | (def 'defn (macro ['name 'params 'body] (def name (fn params body))))
        | (defn 'id ['x] x)
        | (id 69) 
        | (id "hello")
        | (defn 'plus ['a 'b] (+ a b))
        | (plus 8 61)
        | (id "lol")
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Nil, Num(69), Str("hello"), Nil, Num(69), Str("lol")), scopes)
  }

  testOne("defn defined as a macro -> functions are applicable 2") {
    (
      p("""
         | (def 'defn (macro ['name 'params 'body] (def name (fn params body))))
         | (defn 'foo1 ['x 'y] x)
         | (defn 'foo2 ['a 'b] b)
         | (foo1 69 420)
         | (foo2 "hello" "world")
         | (foo1 420 69)
         | (foo2 "world" "hello")
         |""".stripMargin),
      scopes
    ) -> (List(Nil, Nil, Nil, Num(69), Str("world"), Num(420), Str("hello")), scopes)
  }

  testOne("defn defined as a macro -> functions are applicable 3") {
    (
      p("""
        | (def 'defn (macro ['name 'params 'body] (def name (fn params body))))
        | (defn 'id ['x] x)
        | (id 69)
        | (defn 'double ['x] (x x))
        | (double (id (+ (id 10) 23)))
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Nil, Num(69), Nil, List(Num(33), Num(33))), scopes)
  }

  testOne("defm defined as macro and defn defined as defm -> function is applicable") {
    (
      p("""
        | (def 'defm (macro ['name 'params 'body] (def name (macro params body))))
        | (defm 'defn ['name 'params 'body] (def name (fn params body)))
        | (defn 'id ['x] x)
        | (id 69)
        | (defn 'double ['x] (x x))
        | (double (id (+ (id 10) 23)))
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Nil, Nil, Num(69), Nil, List(Num(33), Num(33))), scopes)
  }

  scopes = Scope(Sym("id") -> Fn(List.sym(Sym("x")), Sym("x"))) :: scopes

  testOne("id in scope (num) -> Num") { (p("(id 69)"), scopes) -> (Num(69), scopes) }
  testOne("id in scope ('Sym) -> Sym") { (p("(id 'foo)"), scopes) -> (Sym("foo"), scopes) }
  testOne("id in scope (id) -> id") { (p("(id id)"), scopes) -> (Fn(List.sym(Sym("x")), Sym("x")), scopes) }

  testOne("apply call with id -> same value") {
    (p("(def 'apply (fn ['f 'v] (f v))) (apply id 69)"), scopes) -> (List(Nil, Num(69)), scopes)
  }

  testOne("(fn ['a 'b] (+ a b)) -> correct function definition") {
    (p("(fn ['a 'b] (+ a b))"), scopes) ->
    (Fn(
      List.sym(Sym("a"), Sym("b")),
      List(Sym("+"), Sym("a"), Sym("b"))
    ), scopes)
  }

  testOne("plus with vararg -> correctly calculates result") {
    (
      p("""
        | (def 'plus (fn ['...args] (+ args)))
        | (plus 1 2 3 4 5 10 20)
        | (plus "h" "e" "llo" " " "wor" "l" "d" "")
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Num(45), Str("hello world")), scopes)
  }

  testOne("""(+ 1 2 3) -> 6""") { (p("(+ 1 2 3)"), scopes) -> (Num(6), scopes) }
  testOne("""(+ (+ 1 2) (+ 3 (+ 4 5) 6)) -> 21""") {
    (p("(+ (+ 1 2) (+ 3 (+ 4 5) 6))"), scopes) -> (Num(21), scopes)
  }

  testOne("sum of 1-10000 -> 50005000") {
    (p(s"""(+ ${(1 to 10000).mkString(" ")})"""), scopes) -> (Num(50005000L), scopes)
  }

  scopes = Scope(Sym("plus") -> Fn(List.sym(Sym("a"), Sym("b")), List(Sym("+"), Sym("a"), Sym("b")))) :: scopes

  testOne("(plus 69 420) -> 489") {
    (p("(plus 69 420)"), scopes) -> (Num(489), scopes)
  }

  testOne("tail-recursive fibonacci is defined and correctly calculates values") {
    (
      p("""
        | (def 'defn (macro ['name 'params 'body] (def name (fn params body))))
        | (defn 'fib ['n]
        |  (last 
        |    (defn 'iter ['i 'a 'b]
        |      (if (< i n) 
        |          (iter (+ i 1) b (+ a b))
        |          a))
        |    (if (= n 1) 0 (iter 0 0 1))))
        | (fib 1)
        | (fib 2)
        | (fib 10)
        | (fib 100)
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Nil, Num(0), Num(1), Num(55), Num(BigDecimal("354224848179261915075"))), scopes)
  }

  testOne("eval evaluates simple expression") {
    (
      p("""
        | (def 'x (fn [] ('+ 1 2)))
        | (x)
        | (eval x)
        |""".stripMargin),
      scopes
    ) -> (List(Nil, List(Sym("+"), Num(1), Num(2)), Num(3)), scopes)
  }

  testOne("eval exposes scopes") {
    (
      p("""
        | (def 'foo1 69)
        | (def 'x (fn [] ('def ''foo2 69)))
        | (eval x)
        | (foo1)
        | (foo2)
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Nil, Nil, Num(69), Num(69)), scopes)
  }

  testOne("simple (+ 45 24) -> 69") {
    (List(Sym("+"), Num(45), Num(24)), scopes) -> (Num(69), scopes)
  }

  testOne("parse parses valid string") {
    (p("""(parse "(+ 45 24)")""".stripMargin), scopes) -> (List(Sym("+"), Num(45), Num(24)), scopes)
  }

  testOne("parse parses valid string and then eval evaluates it") {
    (p("""(eval (parse "(+ 45 24)"))""".stripMargin), scopes) -> (Num(69), scopes)
  }

  testOne("[std] compose composes two single-arg functions") {
    (
      p("""
        | (include std)
        | (defn 'plus10 ['a] (+ a 10))
        | (defn 'plus20 ['a] (+ 20 a))
        | ((compose plus10 plus20) 30)
        | (def 'plus30 (compose plus20 plus10))
        | (plus30 40)
        |""".stripMargin),
      scopes
    ) -> (List(Nil, Nil, Nil, Num(60), Nil, Num(70)), scopes)
  }
