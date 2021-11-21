package dev.vgerasimov.lizp

import scala.collection.mutable

def eval(ctx: Context)(scope: Map[Id, Const | Func], exprs: LazyExpr*): List[Expr] =
  val localScope = mutable.Map[Id, Const | Func]()
  ctx.intrinsics.foreach { case (n, d) => localScope.put(n, d) }
  scope.foreach { case (n, d) => localScope.put(n, d) }

  exprs.toList.map(f => f()).flatMap {
    case v @ LNull   => List(v)
    case v @ LUnit   => List(v)
    case v: LBool => List(v)
    case v: LInt     => List(v)
    case v: LDouble  => List(v)
    case v: LStr  => List(v)
    case d @ Func(name, _, _) =>
      localScope.put(name, d)
      List(LUnit)
    case Const(name, exprs) =>
      val evaluated: Expr = eval(ctx)(localScope.toMap, exprs*).last
      localScope.put(name, Const(name, List(() => evaluated)))
      List(LUnit)
    case Call(name, args) =>
      val defn = localScope.get(name) match
        case Some(d) => d
        case None    => throw new ExecutionError.UnknownReference(name)
      val newScope = localScope.toMap
      defn match
        case const: Const => const.value.map(le => le())
        case definition: Func =>
          val evaluatedArgs: List[LazyExpr] = args.map(arg => () => eval(ctx)(newScope, arg).last)
          val captures: Map[Id, Const | Func] =
            (definition.params zip evaluatedArgs).map({ case (param, lazyFunc) => (param, Const(param, List(lazyFunc))) }).toMap
          eval(ctx)(newScope ++ captures, definition.func(evaluatedArgs)*)
  }

sealed trait ExecutionError extends RuntimeException
object ExecutionError:
  case class UnknownReference(id: Id) extends ExecutionError:
    override val getMessage: String = s"Unknown reference: ${id.v}"
  case class UnexpectedDefinition() extends ExecutionError

class Context:
  val intrinsics: Map[Id, Const | Func] = Map(
    Id("<") -> Func(
      Id("<"),
      List(Id("a"), Id("b")),
      ls => List(() => LBool(ls(0)().asInstanceOf[LInt].v < ls(1)().asInstanceOf[LInt].v))
    ),
    Id(">=") -> Func(
      Id(">="),
      List(Id("a"), Id("b")),
      ls => List(() => LBool(ls(0)().asInstanceOf[LInt].v >= ls(1)().asInstanceOf[LInt].v))
    ),
    Id("=") -> Func(
      Id("="),
      List(Id("a"), Id("b")),
      ls => List(() => LBool(ls(0)().asInstanceOf[LInt].v == ls(1)().asInstanceOf[LInt].v))
    ),
    Id("+") -> Func(
      Id("+"),
      List(Id("a"), Id("b")),
      ls => List(() => LInt(ls(0)().asInstanceOf[LInt].v + ls(1)().asInstanceOf[LInt].v))
    ),
    Id("%") -> Func(
      Id("%"),
      List(Id("a"), Id("b")),
      ls => List(() => LInt(ls(0)().asInstanceOf[LInt].v % ls(1)().asInstanceOf[LInt].v))
    ),
    Id("if") -> Func(
      Id("if"),
      List(Id("cond"), Id("then"), Id("else")),
      ls => List(if (ls(0)().asInstanceOf[LBool].v) ls(1) else ls(2))
    ),
    Id("println") -> Func(
      Id("println"),
      List(Id("args")),
      ls => {
        ls.foreach { arg =>
          {
            arg() match {
              case LNull   => System.out.print("null")
              case LUnit       => System.out.print("()")
              case LBool(x) => System.out.print(x)
              case LInt(x)  => System.out.print(x)
              case LDouble(x)  => System.out.print(x)
              case LStr(x)  => System.out.print(x)
            }
            System.out.print(" ")
          }
        }
        System.out.println()
        List(() => LUnit)
      }
    )
  )
