package dev.vgerasimov.lizp

import scala.io.Source

import dev.vgerasimov.lizp.Parser.*
import dev.vgerasimov.lizp.syntax.*

@main def run(files: String*) =
  (for {
    source               <- files.map(Source.fromFile).map(_.getLines.mkString("\n")).mkString("\n").asRight
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
