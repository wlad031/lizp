package dev.vgerasimov.lizp

import java.nio.file.{ Path, Paths }

import scala.util.Try

import dev.vgerasimov.lizp.types.*
import dev.vgerasimov.lizp.syntax.*

/** Evaluates given expression. */
trait Interpreter:
  def eval(
    expr: Expr,
    scopes: Interpreter.Scopes = Interpreter.Scopes.withNatives
  )(using
    ctx: Interpreter.Context
  ): Either[LizpError, (Expr, Interpreter.Scopes)]

  def apply(
    expr: Expr,
    scopes: Interpreter.Scopes = Interpreter.Scopes.withNatives
  )(using
    ctx: Interpreter.Context
  ): Either[LizpError, (Expr, Interpreter.Scopes)] = eval(expr, scopes)

/** Contains interpreter-related types and functions. */
object Interpreter:
  import renamings.PrefixScalaTypes.*

  private lazy val impl = InterpreterImpl

  /** Returns implementation of [[Interpreter]]. */
  def apply(): Interpreter = impl

  /** Contains named expressions. */
  type Scope = ScalaMap[Sym, Expr]
  type Scopes = ScalaList[Scope]

  object Scope:
    def empty: Scope = ScalaMap()
    def apply(pairs: (Sym, Expr)*): Scope = ScalaMap[Sym, Expr](pairs*)

  object Scopes:
    def empty: Scopes = ScalaList(Scope.empty)
    def withNatives: Scopes = ScalaList(natives)
    def apply(scopes: Scope*): Scopes = scopes.toList

  extension (scopes: Scopes)
    def flattenReversed: Scope = scopes.reverse.flatten.toMap
    def get(sym: Sym): Option[Expr] = scopes.find(_.contains(sym)).map(_.apply(sym))
    def put(sym: Sym, expr: Expr): Scopes = scopes.tail.prepended(scopes.head + (sym -> expr))
    def show: String = scopes.map(_.show).mkString("\n----------\n") // TODO: get rid of it

  extension (scope: Scope) def show: String = scope.map((k, v) => s"  $k -> $v").mkString("\n") // TODO: get rid of it

  /** Represents generic interpretation error. */
  sealed trait Error extends LizpError

  /** Contains specific interpretation errors. */
  object Error:
    case class InvalidExpression(expr: Expr) extends Error
    case class CannotFindInScopes(scopes: Scopes, sym: Sym) extends Error:
      override def toString: String = s"""
      |Cannot find in scope sym=$sym
      |Scopes:
      |${scopes.show}""".stripMargin // TODO: get rid of it
    case class InvalidFnParams(expr: Expr, evaluated: Expr) extends Error
    case class InvalidScopesState(scopes: Scopes) extends Error

  case class Context(
    debug: Context.DebugMode = Context.DebugMode.None,
    callDepth: Int = 0,
    parser: Parser,
    reader: Reader = Reader(),
    interpreter: Interpreter = impl,
    sourcePaths: ScalaList[Path] = ScalaList(
      Paths.get(classOf[Interpreter].getResource(".").toURI).resolve("../../../")
    ),
    withNoNotes: Boolean = true
  )
  object Context:
    enum DebugMode:
      case None
      case Expr
      case Scopes
      case ExprAndScopes

  val natives: Scope = {
    def err(s: String) = Native.Error(s.stripMargin)
    def $ (p: (String, (List[Expr], Scopes, Context) => Either[String, (Expr, Scopes)])) =
      Sym(p._1) -> Native[Scopes, Context](new Native.Fn {
        override def apply(
          expressions: List[Expr],
          scopes: Scopes,
          ctx: Context
        ): Either[Native.Error, (Expr, Scopes)] =
          p._2.apply(expressions, scopes, ctx).mapLeft(err)
      })
    def compare(
      str: String,
      fnNum: (BigDecimal, BigDecimal) => Boolean,
      fnStr: (String, String) => Boolean,
      fnBool: (Boolean, Boolean) => Boolean
    ) =
      $(
        str -> ((params, scopes, _) =>
          params match {
            case Num(x) :: Num(y) :: Nil   => Bool(fnNum(x, y)).asRight.map((_, scopes))
            case Str(x) :: Str(y) :: Nil   => Bool(fnStr(x, y)).asRight.map((_, scopes))
            case Bool(x) :: Bool(y) :: Nil => Bool(fnBool(x, y)).asRight.map((_, scopes))
            case x :: y :: Nil => s"""
          |Native "$str"
          |  expected 
          |    - str str
          |    - num num
          |    - bool bool
          |  got 
          |    $x $y
          |""".asLeft
            case ls => s"""
          |Native "$str"
          |  expected
          |    2 arguments
          |  got 
          |    "${ls.size}" arguments
          |""".asLeft
          }
        )
      )
    Scope(
      Sym("nil") -> Nil,
      Sym("std") -> Str("std.lz"),
      $(
        "read-file" -> ((params, scopes, ctx) =>
          params match
            case Str(path) :: Nil =>
              ctx.reader
                .apply(path)()
                .map(Str.apply)
                .map((_, scopes))
                .mapLeft(readError => s"""
        |Native "read-file"
        |  cannot read file $path
        |  error
        |    $readError
        |""")
            case expr => s"""
        |Native "read-file"
        |  expected
        |    str
        |  got
        |    $expr
        |""".asLeft
        )
      ),
      $(
        "include" -> ((params, _, ctx) =>
          params match
            case Str(path) :: Nil =>
              given implicitCtx: Context = ctx
              ctx.reader
                .apply(path)(ctx.sourcePaths)
                .flatMap(ctx.parser)
                .flatMap(expr => ctx.interpreter.eval(expr, Scopes.withNatives))
                .map((_, newScopes) => ((if (ctx.withNoNotes) Nil else Note(s"""Included "$path"""")), newScopes))
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
      $(
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
      $(
        "size" -> ((ls, scopes, _) =>
          ls match {
            case (ls: List[?]) :: Nil => Num(ls.size).asRight.map((_, scopes))
            case ls                   => Num(ls.size).asRight.map((_, scopes))
          }
        )
      ),
      $(
        "last" -> ((params, scopes, _) =>
          params match {
            case (ls: List[?]) :: Nil => ls.last.asRight.map((_, scopes))
            case ls: List[?]          => ls.last.asRight.map((_, scopes))
          }
        )
      ),
      $(
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
      $(
        "+" -> ((params, scopes, _) =>
          params match {
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
                .map(_.asInstanceOf[Str])
                .map(_.value)
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
                .map(_.asInstanceOf[Str])
                .map(_.value)
                .foldLeft(x)(_ + _)
                .asRight
                .map(Str.apply)
                .map((_, scopes))
            case expr => s"""Native "+": unsupported parameters "$expr"""".asLeft
          }
        )
      ),
      $(
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
  }

/** Simple implementation of [[Interpreter]]. */
private object InterpreterImpl extends Interpreter:
  import Interpreter.*
  import renamings.PrefixScalaTypes.*

  override def eval(expr: Expr, scopes: Scopes)(using ctx: Context): Either[LizpError, (Expr, Scopes)] = {
    given Context = ctx.copy(callDepth = ctx.callDepth + 1)
    debug(expr, scopes)
    val res: Either[LizpError, (Expr, Scopes)] = expr match
      case atom: Atom     => (atom, scopes).asRight
      case Sym.Fn         => (Keyword.Fn, scopes).asRight
      case Sym.Def        => (Keyword.Def, scopes).asRight
      case Sym.List       => (Keyword.List, scopes).asRight
      case Sym.If         => (Keyword.If, scopes).asRight
      case Sym.Eval       => (Keyword.Eval, scopes).asRight
      case Sym.Macro      => (Keyword.Macro, scopes).asRight
      case sym @ Sym(ref) =>
        // TODO: refactor this
        if (ref.startsWith("'")) (Sym(ref.substring(1)), scopes).asRight
        else eval(Deref(sym), scopes)
      case deref: Deref       => eval(Apply(deref, Nil), scopes)
      case deref: EvalOnDeref => (deref, scopes).asRight
      case fn: Fn             => (fn, scopes).asRight
      case Apply((atom: Atom), Nil) =>
        (atom, scopes).asRight
      case Apply((atom: Atom), args) =>
        evalIndividually(args, scopes)
          .map({ case (ls: List[?], _) =>
            (atom :: ls, scopes)
          })
      case Apply(Nil, args) =>
        evalIndividually(args, scopes).map({ case (ls: List[?], x) => (Nil :: ls, x) })
      case Apply(note: Note, args) =>
        evalIndividually(args, scopes).map({ case (ls: List[?], x) => ((if (ctx.withNoNotes) Nil else note) :: ls, x) })
      case Apply((sym: Sym), args) =>
        evalIndividually(args, scopes).map({ case (ls: List[?], _) => (sym :: ls, scopes) })
      case Apply(Keyword.List, ls: List[?]) =>
        evalIndividually(ls, scopes).map({ case (expr, _) => (expr, scopes) })
      case Apply(Keyword.If, cond :: thenExpr :: elseExpr :: Nil) =>
        eval(cond, scopes)
          .flatMap({
            case (eCond: Bool, _) => eval(if (eCond.value) thenExpr else elseExpr, scopes)
            case _                => sys.error("some error") // TODO: implement this
          })

      case defExpr @ Apply(Keyword.Def, list) =>
        eval(list, scopes)
          .flatMap({
            case ((eSym: Sym) :: (eExpr: Expr) :: Nil, _) =>
              ((if (ctx.withNoNotes) Nil else Note(s"""Defined "${eSym.value}"""")), scopes.put(eSym, eExpr)).asRight
            case _ => Error.InvalidExpression(defExpr).asLeft
          })

      case Apply(Keyword.Macro, params :: body :: Nil) =>
        eval(params, scopes)
          .flatMap({
            case (eParams: List[?], _) => (Macro(eParams.asInstanceOf[List[Sym]], body), scopes).asRight
            case (x, _)                => Error.InvalidFnParams(params, x).asLeft
          })

      case Apply(Keyword.Fn, params :: body :: Nil) =>
        eval(params, scopes)
          .flatMap({
            case (eParams: List[?], _) =>
              Try(eParams.asInstanceOf[List[Sym]])
                .map(eSymParams => (Fn(eSymParams, body), scopes))
                .toEither
                .mapLeft(_ => Error.InvalidFnParams(params, eParams))
            case (x, _) => Error.InvalidFnParams(params, x).asLeft
          })

      case Apply(Deref(sym), _) =>
        scopes
          .get(sym)
          .toRight(Error.CannotFindInScopes(scopes, sym))
          .flatMap({
            case EvalOnDeref(expr) => eval(expr, scopes)
            case expr              => (expr, scopes).asRight
          })

      case Apply(Native(fn), args) =>
        evalIndividually(args, scopes)
          .map(_._1)
          .flatMap(fn.asInstanceOf[Native.Fn[Scopes, Context]].apply(_, scopes, ctx))

      case Apply(Keyword.Eval, args) =>
        eval(args, scopes).flatMap(x => {
          eval(x._1, x._2)
        })

      case Apply(m @ Macro(params, body), args) =>
        eval(replace(body, (params.asScala zip args.asScala).toMap), scopes)

      case Apply(fn @ Fn(syms, body), args) =>
        // fn has some parameters, but no arguments provided, so we return the same function as a result
        if (args.size == 0 && syms.size > 0) (fn, scopes).asRight
        // fn has some parameters, but not enough arguments provided, so we build a lambda as a result
        else if (args.size < syms.size) ??? // TODO: implement
        // fn has parameters and all required arguments are provided, we evaluate this function
        // and drop all extra arguments
        else
          val params: ScalaList[Param] = syms.asScala.map(Param.apply)
          val eArgsOrError: Either[LizpError, List[Expr]] =
            evalIndividually(
              params
                .zipAll(args.asScala, null, null)
                .foldLeft(ScalaList[Expr]())((res, x) => {
                  x match
                    case (_: Param.ByValue, arg) => ScalaCons(arg, res)
                    case (_: Param.ByName, arg)  => ScalaCons(EvalOnDeref(arg), res)
                    case (_: Param.VarArg, arg)  => ScalaCons(List(arg), res)
                    // FIXME: WTF???
                    case (null, arg) =>
                      ScalaCons(
                        (arg :: res.head.asInstanceOf[List[Expr]].asScala.reverse.asLizp).asScala.reverse.asLizp,
                        res.tail
                      )
                })
                .reverse
                .asLizp,
              scopes
            )
              // drop the arguments evaluating scope
              .map(_._1)

          eArgsOrError
            .map(eArgs =>
              (params zip eArgs.asScala).foldRight(Scope.empty)((paramWithEArg, paramScope) =>
                paramWithEArg match
                  // FIXME: something is wrong here
                  // no need to capture scopes for already evaluated by-value argument
                  case (Param.ByValue(sym), eArg) => paramScope + (sym -> eArg)
                  // capture an entire scopes for by-name argument
                  case (Param.ByName(sym), eArg)                => paramScope + (sym -> eArg)
                  case (Param.VarArg(Param.ByValue(sym)), eArg) => paramScope + (sym -> eArg)
                  case (Param.VarArg(Param.ByName(sym)), eArg)  => paramScope + (sym -> eArg)
                  case _                                        => ???
              )
            )
            .map(paramScope => paramScope :: scopes)
            .flatMap(scope => eval(body, scope))
            // drop the private fn scope
            .map({ case (expr, _) => (expr, scopes) })

      case Apply(target :: args, Nil) =>
        eval(target, scopes)
          .map({ case (expr, scopes) => (Apply(expr, args), scopes) })
          .flatMap({ case (expr, scopes) => eval(expr, scopes) })
      case Nil => (Nil, scopes).asRight
      case target :: args =>
        eval(target, scopes)
          .map({ case (expr, scopes) => (Apply(expr, args), scopes) })
          .flatMap({ case (expr, scopes) => eval(expr, scopes) })
      case expr => Error.InvalidExpression(expr).asLeft
    res.map(r => {
      debug(r._1, r._2, "=> ")
      r
    })
  }

  private def evalIndividually(
    list: List[Expr],
    scopes: Scopes
  )(using
    ctx: Context
  ): Either[LizpError, (List[Expr], Scopes)] =
    list.asScala
      .foldLeft[(Boolean, Either[LizpError, ScalaList[Expr]], Scopes)]((false, Right(ScalaNil), scopes))((acc, x) =>
        (acc, x) match
          case (x @ (true, _, _), _) => x // do not evaluate next expressions if one already failed
          case ((false, accErrOrExpr, accScopes), expr) =>
            eval(expr, accScopes) match
              case Right((eExpr, eScopes)) => (false, accErrOrExpr.map(eExpr :: _), eScopes)
              case Left(error)             => (true, Left(error), accScopes)
      ) match
      case (_, either, scopes) => either.map(_.reverse).map(_.asLizp).map((_, scopes))

  // TODO: refactor this function
  private def replace(expr: Expr, replacements: Map[Sym, Expr]): Expr = expr match
    case sym: Sym => replacements.getOrElse(sym, sym)
    case Nil      => Nil
    case x :: xs  => replace(x, replacements) :: replace(xs, replacements).asInstanceOf[List[Expr]]
    case expr     => expr

  // private def optimize(expression: Expr): Either[LizpError, Expr] =
  //   expression match
  //     case f @ Def(id, Fn(params, body)) =>
  //       body match
  //         case If(cond, Deref(callId, args), elseExpr) if id == callId =>
  //           optimizeTailRecFunc(id, params, args, cond, elseExpr).asRight
  //         case If(cond, thenExpr, Application(callId, args)) if id == callId =>
  //           optimizeTailRecFunc(id, params, args, Application(Sym("not"), List(cond)), thenExpr).asRight
  //         case _ =>
  //           optimize(body.last).map(expressions => Def(id, Lambda(params, List(expressions))))
  //     case expr => expr.asRight

  // private def optimizeTailRecFunc(id: Sym, params: List[FuncParam], args: List[Expr], cond: Expr, result: Expr): Expr =
  //   val ls = (params zip args).map({
  //     case (p, a) => {
  //       val id = Sym(s"${p.name.value}${'$'}0")
  //       (
  //         Def(id, LUnit),
  //         Def(id, Redef(a)),
  //         Def(p.name, Redef(Application(id, Nil)))
  //       )
  //     }
  //   })
  //   Def(id, Fn(params, ls.map(_._1) ++ List(While(cond, ls.map(_._2) ++ ls.map(_._3)), result)))

  private def debug(expr: Expr, scopes: Scopes, prefix: String = "")(using ctx: Context): Unit = ctx.debug match
    case Context.DebugMode.None =>
    case Context.DebugMode.Expr =>
      println(s"""[DEBUG] ${"  ".repeat(ctx.callDepth)}| $prefix$expr""")
    case Context.DebugMode.Scopes =>
      println(s"""[DEBUB] ${scopes.show}""")
    case Context.DebugMode.ExprAndScopes =>
      println(s"""[DEBUG] ${"  ".repeat(ctx.callDepth)}| $prefix$expr
                |         scopes: ${scopes.show}""".stripMargin)
