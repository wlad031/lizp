package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.Parser.*

@main def run =
  val source =
    """|(def fib (n)
       |  (def iter (i a b)
       |    (if (< (i) (n)) 
       |        (iter (+ (i) 1) (b) (+ (a) (b)))
       |        (a)))
       |  (if (= (n) 1) 0 (iter 0 0 1)))
       |(fib 1)
       |(fib 2)
       |(fib 10)
       |(fib 100)
       |""".stripMargin

  (for {
    parsedExpressions    <- parse(source)
    expandedExpressions  <- expand(parsedExpressions)
    optimizedExpressions <- optimize(expandedExpressions)
    result               <- eval(List((new Context()).intrinsics), optimizedExpressions)
  } yield result) match
    case Left(error) =>
      Console.err.println(s"""|Error:
                              |$error""".stripMargin)
    case Right(result) =>
      Console.out.println(s"""|Result:
                              |${result.map(r => "> " + r.toString).mkString("\n")}""".stripMargin)
