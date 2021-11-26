package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.Parser.*

@main def run =
  val source =
    """|(def fact (n i res)
      |  (if (< (i) (n)) (fact (n) (+ 1 (i)) (* (i) (res))) (res)))
      |(fact 10 0 1)
      |(def fib (n i a b)
      |  (if (< (i) (n)) (fib (n) (+ 1 (i)) (b) (+ (a) (b))) (b)))
      |(fib 2 0 0 1)
      |(fib 3 0 0 1)
      |(fib 4 0 0 1)
      |(fib 5 0 0 1)
      |(fib 6 0 0 1)
      |(fib 7 0 0 1)
      |(fib 8 0 0 1)
      |(fib 9 0 0 1)
      |(fib 10 0 0 1)
      |""".stripMargin

  Parser(source) match
    case ParsingError(message) =>
      Console.err.println(s"""|Parsing error:
                              |$message""".stripMargin)
    case exprs: List[Expr] =>
      val optimizedExpressions: List[Expr] = optimize(exprs)
      eval(List((new Context()).intrinsics), optimizedExpressions) match
        case Left(error) =>
          Console.err.println(s"""|Execution error:
                                  |$error""".stripMargin)
        case Right(result) =>
          Console.out.println(s"""|Result:
                                  |${result.map(r => "> " + r.toString).mkString("\n")}""".stripMargin)
