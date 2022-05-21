package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.build.BuildInfo
import dev.vgerasimov.lizp.renamings.PrefixLizpTypes.*
import dev.vgerasimov.lizp.syntax.{ *, given }
import dev.vgerasimov.lizp.types.Expr

@main def run(args: String*) =
  val result = for {
    config <- ArgsParser()(args.toArray)
    sourcePath: SourcePath = config.sourcePaths.map(Source.apply).toSet ++ Set(
      Source.resource("std.lz"),
      Source.directory(".")
    )
    reader = Reader()
    read <-
      if (config.expr != null) config.expr.asRight
      else if (config.file != null) reader(config.file)(sourcePath)
      else "".asRight
    parser = Parser()
    parsed <- parser(read)
    context = Interpreter.Context(
      reader = reader,
      parser = parser,
      sourcePaths = sourcePath,
      withNoNotes = config.verbosity == LizpConfig.Verbosity.Default
    )
    evaluated <- {
      given Interpreter.Context = context
      Interpreter()(parsed)
    }
    res <-
      if (!config.isRepl) evaluated.asRight
      else {
        (evaluated._2, Repl()(Repl.lizpHandler(Interpreter(), context))).asRight
      }
  } yield res

  result match
    case Left(error) =>
      Console.err.println(s"""|Error:
                              |$error""".stripMargin)
    // sys.exit(1)
    case Right((x: List[?], (fn: Function1[?, ?]))) => fn.asInstanceOf[Function1[Any, Any]](x)
    case Right((exprs: Expr, _: List[?])) =>
      Console.out.println(s"""|Result:
                              |${exprs
        .asInstanceOf[LizpList[Expr]]
        .asScala
        .map(r => "> " + r.toString)
        .mkString("\n")}""".stripMargin)
    case x => sys.error(x.toString)
