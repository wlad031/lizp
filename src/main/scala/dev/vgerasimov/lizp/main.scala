package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.Parser.*
import dev.vgerasimov.lizp.syntax.*

@main def run(files: String*) =
  (for {
    source               <- files.map(readScript).toList.partitionToEither.map(_.mkString("\n"))
    parsedExpressions    <- parse(source)
    expandedExpressions  <- expand(parsedExpressions)
    optimizedExpressions <- optimize(expandedExpressions)
    result <- {
      val ctx = new Context(readScript, parse, expand, optimize)
      eval(Nil, optimizedExpressions, ctx)
    }
  } yield result) match
    case Left(error) =>
      Console.err.println(s"""|Error:
                              |$error""".stripMargin)
    case Right(result) =>
      Console.out.println(s"""|Result:
                              |${result.map(r => "> " + r.toString).mkString("\n")}""".stripMargin)
