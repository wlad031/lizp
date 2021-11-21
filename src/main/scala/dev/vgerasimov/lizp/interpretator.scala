package dev.vgerasimov.lizp

import scala.collection.mutable

def eval(ctx: Context)(scope: Map[String, Def], exprs: (() => Expr)*): List[Expr] =
  val localScope = mutable.Map[String, Def]()
  ctx.intrinsics.foreach { case (n, d) => localScope.put(n, d) }
  scope.foreach { case (n, d) => localScope.put(n, d) }

  exprs.toList.map(f => f()).flatMap {
    case v @ ValUnit   => List(v)
    case v: ValInt     => List(v)
    case v: ValString  => List(v)
    case v: ValBoolean => List(v)
    case v: ValDouble  => List(v)
    case d @ Def(name, _, _) =>
      localScope.put(name, d)
      List(ValUnit)
    case Call(name, args) =>
      val definition = localScope.get(name) match
        case Some(d) => d
        case None    => throw new ExecutionError.UnknownReference(name)
      val newScope = localScope.toMap
      val evaluatedArgs: List[() => Expr] = args.map(arg => () => eval(ctx)(newScope, arg).last)
      val captures: Map[String, Def] =
        (definition.params zip evaluatedArgs.map(func => Def("", Nil, _ => List(func)))).toMap
      eval(ctx)(newScope ++ captures, definition.func(evaluatedArgs)*)
  }

sealed trait ExecutionError extends RuntimeException
object ExecutionError:
  case class WrongArgsNumber(name: String, expected: Int, got: Int) extends ExecutionError:
    override val getMessage: String = s"$name expects $expected arguments, but got $got"
  case class UnknownReference(name: String) extends ExecutionError:
    override val getMessage: String = s"Unknown reference: $name"
  case class UnexpectedDefinition() extends ExecutionError
  case class IncompatibleType(expected: String, got: String) extends ExecutionError:
    override val getMessage: String = s"Expected: $expected, got: $got"

class Context:
  val intrinsics: Map[String, Def] = Map(
    "<" -> Def(
      "<",
      List("a", "b"),
      ls => {
        List(() => ValBoolean(ls(0)().asInstanceOf[ValInt].v < ls(1)().asInstanceOf[ValInt].v))
      }
    ),
    ">=" -> Def(
      ">=",
      List("a", "b"),
      ls => {
        List(() => ValBoolean(ls(0)().asInstanceOf[ValInt].v >= ls(1)().asInstanceOf[ValInt].v))
      }
    ),
    "+" -> Def(
      "+",
      List("a", "b"),
      ls => List(() => ValInt(ls(0)().asInstanceOf[ValInt].v + ls(1)().asInstanceOf[ValInt].v))
    ),
    "if" -> Def(
      "if",
      List("cond", "then", "else"),
      ls => {
        List(if (ls(0)().asInstanceOf[ValBoolean].v) ls(1) else ls(2))
      }
    ),
    "println" -> Def(
      "println",
      List("args"),
      ls => {
        ls.foreach { arg =>
          {
            arg() match {
              case ValInt(x)     => System.out.print(x)
              case ValString(x)  => System.out.print(x)
              case ValDouble(x)  => System.out.print(x)
              case ValBoolean(x) => System.out.print(x)
              case ValUnit       => System.out.print("()")
            }
            System.out.print(" ")
          }
        }
        System.out.println()
        List(() => ValUnit)
      }
    )
  )
