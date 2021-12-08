package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*

import scala.collection.mutable

private type Scopes = List[mutable.Map[Sym, Definition]]

extension (scopes: Scopes)
  def get(id: Sym): Option[Definition] = scopes.find(_.contains(id)).map(_.apply(id))
  def update(id: Sym, definition: Definition): Unit =
    def iter(scopes: Scopes): Unit =
      scopes match
        case Nil                              => throw ExecutionError.UnknownDefinition(id)
        case scope :: _ if scope.contains(id) => scope += (id -> definition)
        case _ :: tail                        => iter(tail)
    iter(scopes)
  def show: Unit =
    val reversedScopes = scopes.reverse
    for (i <- 0 until scopes.size) yield
      val margin = "|" + " ".repeat(i * 2)
      val currentScope = scopes(scopes.length - i - 1)
      currentScope.foreach((id, definition) =>
        definition match
          case Redef(_, exprs) => println(s"$margin${id.value} -> ${exprs}")
          case Const(_, exprs) => println(s"$margin${id.value} -> ${exprs}")
          case _: NativeFunc   => println(s"$margin${id.value} -> native func")
          case Func(_, params, exprs) =>
            println(s"""$margin${id.value} -> (${params
              .map(p => ((if (p.isLazy) "=>" else "") + p.name.value))
              .mkString(", ")}) -> ${exprs}""")
      )

def eval(scopes: Scopes, expressions: List[Expr]): Either[LizpError, List[Expr]] =
  val localScope = mutable.Map[Sym, Definition]()
  def putToLocalScope(id: Sym, definition: Definition): LUnit.type =
    localScope.put(id, definition)
    LUnit
  def updateInScopes(scopes: Scopes, id: Sym, definition: Definition): LUnit.type =
    scopes.update(id, definition)
    LUnit

  expressions
    .map({
      case literal: Literal   => literal.asRight
      case lambda: Lambda     => lambda.asRight
      case LList(expressions) => eval(localScope :: scopes, expressions).map(LList(_))
      case definition: Definition =>
        definition match
          case func @ NativeFunc(name, _) => putToLocalScope(name, func).asRight
          case func @ Func(name, _, _)    => putToLocalScope(name, func).asRight
          case Const(name, expression) =>
            eval(localScope :: scopes, List(expression))
              .map(_.last)
              .map(evaluated => putToLocalScope(name, Const(name, evaluated)))
          case Redef(name, expression) =>
            eval(scopes, List(expression))
              .map(_.last)
              .map(evaluated => updateInScopes(scopes, name, Const(name, evaluated)))
      case If(condition, thenExpr, elseExpr) =>
        eval(localScope :: scopes, List(condition))
          .map(_.last)
          .flatMap({
            case LUnit | LNull => false.asRight
            case LBool(cond)   => cond.asRight
            case LNum(value)   => (value != 0.0d).asRight
            case LStr(value)   => (value.isEmpty).asRight
            case _             => ExecutionError.WrongArgumentTypes(null, null).asLeft
          })
          .flatMap(condition => eval(localScope :: scopes, List(if (condition) thenExpr else elseExpr)))
          .map(_.last)
      case unsafe: Unsafe =>
        // quite non-functional mutable operations
        import scala.util.control.Breaks.*
        unsafe match
          case While(condition, sideEffects) =>
            val newScopes = localScope :: scopes
            var result: Either[LizpError, List[Expr]] = null
            breakable {
              while (true) do
                eval(newScopes, List(condition))
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
                    result = eval(newScopes, sideEffects)
            }
            if (result == null) sys.error("Unexpected to have result null here")
            result.map(_.last)
      case Call(name, args) =>
        val newScopes = localScope :: scopes
        newScopes
          .get(name)
          .toRight(ExecutionError.UnknownDefinition(name))
          .flatMap({
            case Redef(_, expression) => expression.asRight
            case Const(_, expression) => expression.asRight
            case NativeFunc(name, f) =>
              eval(newScopes, args)
                .flatMap(evaluatedArgs => eval(newScopes, f(evaluatedArgs)))
                .map(_.last)
            case Func(name, params, body) =>
              (params zip args)
                .map({
                  case (FuncParam(_, false), arg) =>
                    eval(newScopes, List(arg)).map(_.last)
                  case (FuncParam(_, true), arg) =>
                    sys.error("Lazy func params are not implemented")
                })
                .partitionToEither
                .mapLeft(LizpError.Multi(_))
                .flatMap(evaluatedArgs => {
                  val paramScope: mutable.Map[Sym, Definition] = mutable.Map()
                  (params zip evaluatedArgs)
                    .foreach({
                      case (param, Lambda(lambdaParams, lambdaBody)) =>
                        paramScope.put(param.name, Func(param.name, lambdaParams, lambdaBody))
                      case (param, arg) => paramScope.put(param.name, Const(param.name, arg))
                    })
                  eval(paramScope :: newScopes, body).map(_.last)
                })
          })
    })
    .partitionToEither
    .mapLeft(LizpError.Multi(_))

sealed trait ExecutionError extends RuntimeException with LizpError
object ExecutionError:
  case class UnknownDefinition(ref: Sym) extends ExecutionError:
    override def toString: String = s"Unknown definition: $ref"
  case class UnexpectedDefinition() extends ExecutionError
  case class WrongArgumentTypes(expected: List[List[String]], got: List[Expr]) extends ExecutionError
  case class LazyArgEvaluation() extends ExecutionError

class Context:
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
