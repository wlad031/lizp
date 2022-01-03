package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*

import scala.annotation.tailrec
import scala.collection.mutable

private type Scopes = List[mutable.Map[Sym, Definition]]

extension (scopes: Scopes)
  def get(id: Sym): Option[Definition] = scopes.find(_.contains(id)).map(_.apply(id))
  def update(id: Sym, definition: Definition): Unit =
    @tailrec def iter(ls: Scopes): Unit =
      ls match
        case Nil                              => scopes.headOption.map(scope => scope += (id -> definition))
        case scope :: _ if scope.contains(id) => scope += (id -> definition)
        case _ :: tail                        => iter(tail)
    iter(scopes)
  def show(): Unit =
    val reversedScopes = scopes.reverse
    for (i <- scopes.indices) yield
      val margin = "|" + " ".repeat(i * 2)
      val currentScope = scopes(scopes.length - i - 1)
      currentScope.foreach((id, definition) =>
        definition match
          case Redef(_, exprs) => println(s"$margin${id.value} -> $exprs")
          case Const(_, exprs) => println(s"$margin${id.value} -> $exprs")
          case _: NativeFunc   => println(s"$margin${id.value} -> native func")
          case Func(_, params, exprs) =>
            println(s"""$margin${id.value} -> (${params
              .map(p => (if (p.isLazy) "=>" else "") + p.name.value)
              .mkString(", ")}) -> $exprs""")
      )

def eval(scopes: Scopes, expressions: List[Expr], ctx: Context): Either[LizpError, List[Expr]] =
  val localScope = mutable.Map[Sym, Definition]()

  def putToLocalScope(id: Sym, definition: Definition): Expr =
    localScope.put(id, definition)
    definition

  def updateInScopes(scopes: Scopes, id: Sym, definition: Definition): Expr =
    scopes.update(id, definition)
    definition

  expressions
    .map({
      case literal: Literal => literal.asRight
      case lambda: Lambda   => lambda.asRight
      case LNil             => LNil.asRight
      case head :+: tail =>
        val newScopes = localScope :: scopes
        for {
          evaluatedHead <- eval(newScopes, List(head), ctx).map(_.last)
          evaluatedTail <- eval(newScopes, List(tail), ctx).map(_.last)
        } yield evaluatedHead :+: evaluatedTail.asInstanceOf[LList]
      case definition: Definition =>
        definition match
          case func @ NativeFunc(name, _) =>
            ctx.natives.get(name) match
              case Some(f) => putToLocalScope(name, f).asRight
              case None    => ExecutionError.NativeNotFound(name).asLeft
          case func @ Func(name, _, _) => putToLocalScope(name, func).asRight
          case Const(name, expression) =>
            eval(localScope :: scopes, List(expression), ctx)
              .map(_.last)
              .map(evaluated => putToLocalScope(name, Const(name, evaluated)))
          case Redef(name, expression) =>
            eval(scopes, List(expression), ctx)
              .map(_.last)
              .map(evaluated => updateInScopes(scopes, name, NativeFunc(name, _ => List(evaluated))))
      case If(condition, thenExpr, elseExpr) =>
        eval(localScope :: scopes, List(condition), ctx)
          .map(_.last)
          .flatMap({
            case LUnit | LNull => false.asRight
            case LBool(cond)   => cond.asRight
            case LNum(value)   => (value != 0.0d).asRight
            case LStr(value)   => (value.isEmpty).asRight
            case _             => ExecutionError.WrongArgumentTypes(null, null).asLeft
          })
          .flatMap(condition => eval(localScope :: scopes, List(if (condition) thenExpr else elseExpr), ctx))
          .map(_.last)
      case unsafe: Unsafe =>
        // quite non-functional mutable operations
        import scala.util.control.Breaks.*
        unsafe match
          case While(condition, sideEffects) =>
            val newScopes = localScope :: scopes
            var result: Either[LizpError, List[Expr]] = null
            breakable {
              while true do
                eval(newScopes, List(condition), ctx)
                  .map(_.last)
                  .flatMap({
                    case LBool(v) => v.asRight
                    case x        => ExecutionError.WrongArgumentTypes(List(List("bool")), List(x)).asLeft
                  }) match
                  case Left(error) =>
                    result = Left(error)
                    break
                  case Right(false) =>
                    result = List(LUnit).asRight
                    break
                  case Right(true) =>
                    result = eval(newScopes, sideEffects, ctx)
            }
            if (result == null) sys.error("Unexpected to have result null here")
            result.map(_.last)
      case Include(file) =>
        for {
          source               <- ctx.readSource(file)
          parsedExpressions    <- ctx.parse(source)
          expandedExpressions  <- ctx.expand(parsedExpressions)
          optimizedExpressions <- ctx.optimize(expandedExpressions.filter(_.isInstanceOf[Definition]))
          evaluatedExpressions <- eval(scopes, optimizedExpressions, ctx)
        } yield evaluatedExpressions.foreach({
          case func @ Func(name, _, _)    => putToLocalScope(name, func)
          case func @ NativeFunc(name, _) => putToLocalScope(name, func)
          case const @ Const(name, _)     => putToLocalScope(name, const)
        })
        LUnit.asRight
      case Call(name, args) =>
        val newScopes = localScope :: scopes
        newScopes
          .get(name)
          .toRight(ExecutionError.UnknownDefinition(name))
          .flatMap({
            case Redef(_, expression) => expression.asRight
            case Const(_, expression) => expression.asRight
            case NativeFunc(name, f) =>
              eval(newScopes, args, ctx)
                .flatMap(evaluatedArgs => eval(newScopes, f(evaluatedArgs), ctx))
                .map(_.last)
            case Func(name, params, body) =>
              (params zip args)
                .map({
                  case (FuncParam(_, false), arg) =>
                    eval(newScopes, List(arg), ctx).map(_.last)
                  case (FuncParam(_, true), arg) =>
                    sys.error("Lazy func params are not implemented")
                })
                .partitionToEither
                .mapLeft(LizpError.Multi.apply)
                .flatMap(evaluatedArgs => {
                  val paramScope: mutable.Map[Sym, Definition] = mutable.Map()
                  (params zip evaluatedArgs)
                    .foreach({
                      case (param, Lambda(lambdaParams, lambdaBody)) =>
                        paramScope.put(param.name, Func(param.name, lambdaParams, lambdaBody))
                      case (param, arg) => paramScope.put(param.name, Const(param.name, arg))
                    })
                  eval(paramScope :: newScopes, body, ctx).map(_.last)
                })
          })
    })
    .partitionToEither
    .mapLeft(LizpError.Multi.apply)

case class EvalResult(expressions: List[Expr], scopes: Scopes)

sealed trait ExecutionError extends RuntimeException with LizpError
object ExecutionError:
  case class UnknownDefinition(ref: Sym) extends ExecutionError:
    override def toString: String = s"Unknown definition: $ref"
  case class UnexpectedDefinition() extends ExecutionError
  case class WrongArgumentTypes(expected: List[List[String]], got: List[Expr]) extends ExecutionError
  case class LazyArgEvaluation() extends ExecutionError
  case class NativeNotFound(ref: Sym) extends ExecutionError

class Context(
  val readSource: String => Either[LizpError, String],
  val parse: String => Either[LizpError, List[Expr]],
  val expand: List[Expr] => Either[LizpError, List[Expr]],
  val optimize: List[Expr] => Either[LizpError, List[Expr]]
):

  val natives: mutable.Map[Sym, Definition] = {
    val scope = mutable.Map[Sym, Definition]()
    native.all
      .map({
        case func @ Func(id, _, _)    => (id, func)
        case const @ Const(id, _)     => (id, const)
        case redef @ Redef(id, _)     => (id, redef)
        case func @ NativeFunc(id, _) => (id, func)
      })
      .foreach({ case (k: Sym, v: Definition) => scope.put(k, v) })
    scope
  }
