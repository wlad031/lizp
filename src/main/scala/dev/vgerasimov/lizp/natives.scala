package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*
import dev.vgerasimov.lizp.types.*
import dev.vgerasimov.lizp.Interpreter.*

val natives: Scope = Scope(
  fn(
    "exit" -> ((params, _, _) =>
      params match
        case Num(exitCode) :: Nil => sys.exit(exitCode.toInt)
        case expr => s"""
        |Native "exit"
        |  expected
        |    num
        |  got
        |    $expr
        |""".asLeft
    )
  ),
  fn("println" -> ((params, scopes, _) =>
    println(params)
    (Nil, scopes).asRight
  )),
  const("nil" -> Nil),
  const("std" -> Str("std.lz")),
  fn(
    "sym->str" -> ((params, scopes, _) =>
      params match
        case Sym(value) :: Nil => Str(value).asRight.map((_, scopes))
        case expr => s"""
        |Native "sym->str"
        |  expected
        |    sym
        |  got
        |    $expr
        |""".asLeft
    )
  ),
  fn(
    "str->sym" -> ((params, scopes, _) =>
      params match
        case Str(value) :: Nil => Sym(value).asRight.map((_, scopes))
        case expr => s"""
        |Native "str->sym"
        |  expected
        |    str
        |  got
        |    $expr
        |""".asLeft
    )
  ),
  fn(
    "include" -> ((params, scopes, ctx) =>
      params match
        case Str(path) :: Nil =>
          if (ctx.included.contains(path)) (Note(s"""Already included "$path""""), scopes).asRight
          else
            given implicitCtx: Context = ctx.copy(included = ctx.included + path)
            ctx.reader
              .apply(path)(ctx.sourcePaths)
              .flatMap(ctx.parser)
              .flatMap(expr => ctx.interpreter.eval(expr, Scopes.withNatives))
              .map((_, newScopes) =>
                ((if (ctx.withNoNotes) Nil else Note(s"""Included "$path"""")), scopes ++ newScopes)
              )
              .mapLeft(readError => s"""
                |Native "include"
                |  cannot include file $path
                |Error:
                |  $readError
                |""")
        case expr => s"""
          |Native "include"
          |  expected
          |    str
          |  got
          |    $expr
          |""".asLeft
    )
  ),
  fn(
    "parse" -> ((params, scopes, ctx) =>
      params match {
        case Str(value) :: Nil =>
          ctx.parser
            .apply(value)
            .map((_, scopes))
            .mapLeft(parseError => s"""
              |Native "parse"
              |  cannot parse value: $value
              |  error
              |    $parseError
              |""")
        case expr => s"""
          |Native "parse"
          |  expected
          |    str
          |  got
          |    $expr
          |""".asLeft
      }
    )
  ),
  fn(
    "size" -> ((ls, scopes, _) =>
      ls match {
        case (ls: List[?]) :: Nil => Num(ls.size).asRight.map((_, scopes))
        case ls                   => Num(ls.size).asRight.map((_, scopes))
      }
    )
  ),
  fn(
    "head" -> ((params, scopes, _) =>
      params match {
        case Nil             => (Nil, scopes).asRight
        case (h :: _) :: Nil => h.asRight.map((_, scopes))
        case (h :: _)        => h.asRight.map((_, scopes))
      }
    )
  ),
  fn(
    "tail" -> ((params, scopes, _) =>
      params match {
        case Nil             => (Nil, scopes).asRight
        case (_ :: t) :: Nil => t.asRight.map((_, scopes))
        case (_ :: t)        => t.asRight.map((_, scopes))
      }
    )
  ),
  fn(
    "last" -> ((params, scopes, _) =>
      params match {
        case (ls: List[?]) :: Nil => ls.last.asRight.map((_, scopes))
        case ls: List[?]          => ls.last.asRight.map((_, scopes))
      }
    )
  ),
  fn(
    "=" -> ((params, scopes, _) =>
      params match {
        case x :: y :: Nil => Bool(x.equals(y)).asRight.map((_, scopes))
        case ls => s"""
        |Native "="
        |  expected
        |    2 arguments
        |  got 
        |    "${ls.size}" arguments
        |""".asLeft
      }
    )
  ),
  compare("<", _ < _, _ < _, _ < _),
  compare("<=", _ <= _, _ <= _, _ <= _),
  compare(">", _ > _, _ > _, _ > _),
  compare(">=", _ >= _, _ >= _, _ >= _),
  fn(
    "+" -> ((params, scopes, _) =>
      params match {
        case Nil => (Nil, scopes).asRight
        case (x: Num) :: Nil => (Fn(List.sym(Sym("arg")), List(Sym("+"), x, Sym("arg"))), scopes).asRight
        case (Num(x) :: tail) :: Nil =>
          tail.asScala
            .map(_.asInstanceOf[Num])
            .map(_.value)
            .foldLeft(x)(_ + _)
            .asRight
            .map(Num.apply)
            .map((_, scopes))
        case (Str(x) :: tail) :: Nil =>
          tail.asScala
            .map(_.toString)
            // .map(_.value)
            .foldLeft(x)(_ + _)
            .asRight
            .map(Str.apply)
            .map((_, scopes))
        case Num(x) :: tail =>
          tail.asScala
            .map(_.asInstanceOf[Num])
            .map(_.value)
            .foldLeft(x)(_ + _)
            .asRight
            .map(Num.apply)
            .map((_, scopes))
        case Str(x) :: tail =>
          tail.asScala
            .map(_.toString)
            // .map(_.value)
            .foldLeft(x)(_ + _)
            .asRight
            .map(Str.apply)
            .map((_, scopes))
        // TODO: TBH I don't understand why compiler thinks this is an unreachable statement
        // case expr => s"""Native "+": unsupported parameters "$expr"""".asLeft
      }
    )
  ),
  fn(
    "-" -> ((params, scopes, _) =>
      params match {
        case Num(x) :: tail =>
          tail.asScala
            .map(_.asInstanceOf[Num])
            .map(_.value)
            .foldLeft(x)(_ - _)
            .asRight
            .map(Num.apply)
            .map((_, scopes))
        case expr => s"""Native "-": unsupported parameters "$expr"""".asLeft
      }
    )
  )
)

// Helper functions

private def err(s: String) = Native.Error(s.stripMargin)
private def const(p: (String, Expr)) = Sym(p._1) -> p._2
private def fn(p: (String, (List[Expr], Scopes, Context) => Either[String, (Expr, Scopes)])) =
  Sym(p._1) -> Native[Scopes, Context](new Native.Fn {
    override def apply(
      expressions: List[Expr],
      scopes: Scopes,
      ctx: Context
    ): Either[Native.Error, (Expr, Scopes)] =
      p._2.apply(expressions, scopes, ctx).mapLeft(err)
  })

private def compare(
  sym: String,
  fnNum: (BigDecimal, BigDecimal) => Boolean,
  fnStr: (String, String) => Boolean,
  fnBool: (Boolean, Boolean) => Boolean
) =
  fn(
    sym -> ((params, scopes, _) =>
      params match {
        case Num(x) :: Num(y) :: Nil   => Bool(fnNum(x, y)).asRight.map((_, scopes))
        case Str(x) :: Str(y) :: Nil   => Bool(fnStr(x, y)).asRight.map((_, scopes))
        case Bool(x) :: Bool(y) :: Nil => Bool(fnBool(x, y)).asRight.map((_, scopes))
        case x :: y :: Nil => s"""
          |Native "$sym"
          |  expected 
          |    - str str
          |    - num num
          |    - bool bool
          |  got 
          |    $x $y
          |""".asLeft
        case ls => s"""
          |Native "$sym"
          |  expected
          |    2 arguments
          |  got 
          |    "${ls.size}" arguments
          |""".asLeft
      }
    )
  )
