package dev.vgerasimov.lizp

import java.nio.file.{ Path, Paths }

import dev.vgerasimov.lizp.build.BuildInfo
import dev.vgerasimov.lizp.renamings.PrefixLizpTypes.*
import dev.vgerasimov.lizp.syntax.{ *, given }
import dev.vgerasimov.lizp.types.Expr

@main def run(args: String*) =
  val result = for {
    config <- ArgsParser()(args.toArray)
    sourcePaths = Paths.get(".") :: config.sourcePaths.toList
    reader = Reader()
    read <- reader(config.file)(sourcePaths)
    parser = Parser()
    parsed <- parser(read)
    context = Interpreter.Context(
      reader = reader,
      parser = parser,
      sourcePaths = sourcePaths,
      withNoNotes = config.verbosity == LizpConfig.Verbosity.Default
    )
    evaluated <- {
      given Interpreter.Context = context
      Interpreter()(parsed)
    }
  } yield evaluated

  result match
    case Left(error) =>
      Console.err.println(s"""|Error:
                              |$error""".stripMargin)
      sys.exit(1)
    case Right((exprs, _)) =>
      Console.out.println(s"""|Result:
                              |${exprs
        .asInstanceOf[LizpList[Expr]]
        .asScala
        .map(r => "> " + r.toString)
        .mkString("\n")}""".stripMargin)
