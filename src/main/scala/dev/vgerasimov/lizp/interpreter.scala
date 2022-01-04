package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*

import scala.annotation.tailrec
import scala.collection.mutable

private type Scopes = List[mutable.Map[Sym, Expr]]

extension (scopes: Scopes)
  def get(id: Sym): Option[Expr] = scopes.find(_.contains(id)).map(_.apply(id))
  def update(id: Sym, definition: Expr): Unit =
    @tailrec def iter(ls: Scopes): Unit =
      ls match
        case Nil                              => scopes.headOption.map(scope => scope += (id -> definition))
        case scope :: _ if scope.contains(id) => scope += (id -> definition)
        case _ :: tail                        => iter(tail)
    iter(scopes)

def eval(scopes: Scopes, expressions: List[Expr], ctx: Context): Either[LizpError, List[Expr]] =
  lazy val localScope = mutable.Map[Sym, Expr]()

  def putToLocalScope(definition: Def): Def =
    localScope.put(definition.ref, definition.expression)
    definition

  def updateInScopes(scopes: Scopes, definition: Def): Def =
    scopes.update(definition.ref, definition.expression)
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
      case definition @ Def(name, expression) =>
        expression match
          case _: NativeFunc =>
            ctx.natives.get(name) match
              case Some(NativeFunc(func)) => putToLocalScope(Def(name, NativeFunc(func))).asRight
              case Some(_)                => putToLocalScope(definition).asRight
              case None                   => ExecutionError.NativeNotFound(name).asLeft
          case _: Lambda => putToLocalScope(definition).asRight
          case Redef(redefExpression) =>
            eval(scopes, List(redefExpression), ctx)
              .map(_.last)
              .map(evaluated => updateInScopes(scopes, Def(name, evaluated)))
          case expression =>
            eval(localScope :: scopes, List(expression), ctx)
              .map(_.last)
              .map(evaluated => putToLocalScope(Def(name, evaluated)))
      case If(condition, thenExpr, elseExpr) =>
        eval(localScope :: scopes, List(condition), ctx)
          .map(_.last)
          .flatMap({
            case LUnit | LNull => false.asRight
            case LBool(cond)   => cond.asRight
            case LNum(value)   => (value != 0.0d).asRight
            case LStr(value)   => (value.isEmpty).asRight
            case x =>
              ExecutionError
                .WrongArgumentTypes(
                  s"""Condition of the "if" expression must evaluate into "bool"/"number"/"string"/"unit"/"null", but got "$x""""
                )
                .asLeft
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
                    case x =>
                      ExecutionError
                        .WrongArgumentTypes(
                          s"""Condition of the "while" expression must evaluate into "bool", but got "$x""""
                        )
                        .asLeft
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
        // quite non-functional mutable operations
        for {
          source               <- ctx.readSource(file)
          parsedExpressions    <- ctx.parse(source)
          expandedExpressions  <- ctx.expand(parsedExpressions)
          optimizedExpressions <- ctx.optimize(expandedExpressions.filter(_.isInstanceOf[Def]))
          evaluatedExpressions <- eval(scopes, optimizedExpressions, ctx)
        } yield evaluatedExpressions.foreach({
          case definition: Def => updateInScopes(localScope :: scopes, definition)
          case _               =>
        })
        LUnit.asRight
      case Application(name, args) =>
        val newScopes = localScope :: scopes
        newScopes
          .get(name)
          .toRight(ExecutionError.UnknownDefinition(name))
          .flatMap({
            case NativeFunc(func) =>
              eval(newScopes, args, ctx)
                .flatMap(evaluatedArgs => eval(newScopes, func(evaluatedArgs), ctx))
                .map(_.last)
            case Lambda(params, body) =>
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
                  val paramScope: mutable.Map[Sym, Expr] = mutable.Map()
                  (params zip evaluatedArgs)
                    .foreach({
                      case (param, Lambda(lambdaParams, lambdaBody)) =>
                        paramScope.put(param.name, Lambda(lambdaParams, lambdaBody))
                      case (param, arg) => paramScope.put(param.name, arg)
                    })
                  eval(paramScope :: newScopes, body, ctx).map(_.last)
                })
            case expression => expression.asRight
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
  case class WrongArgumentTypes(message: String) extends ExecutionError:
    override def toString: String = s"Wrong argument types: $message"
  case class LazyArgEvaluation() extends ExecutionError
  case class NativeNotFound(ref: Sym) extends ExecutionError

class Context(
  val readSource: String => Either[LizpError, String],
  val parse: String => Either[LizpError, List[Expr]],
  val expand: List[Expr] => Either[LizpError, List[Expr]],
  val optimize: List[Expr] => Either[LizpError, List[Expr]]
):

  val natives: mutable.Map[Sym, Expr] = {
    val scope = mutable.Map[Sym, Expr]()
    native.all.foreach(ref => scope.put(ref.ref, ref.expression))
    scope
  }
