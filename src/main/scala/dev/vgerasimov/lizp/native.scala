package dev.vgerasimov.lizp

private[lizp] object native:
  given Conversion[String, Sym] = Sym(_)
  given Conversion[Sym, FuncParam] = FuncParam(_, isLazy = false)
  given Conversion[String, FuncParam] with
    def apply(string: String): FuncParam = FuncParam(Sym(string), isLazy = false)

  import ExecutionError.*

  lazy val all: List[Definition] = List(
    nth,
    greaterOrEqual,
    less,
    equal,
    plus,
    mult,
    printlnFunc
  )

  val nth = NativeFunc(
    "nth",
    ls =>
      (ls(0), ls(1)) match
        case (LNum(n), LList(ls)) => List(ls(n.toInt))
        case (x, y)               => throw WrongArgumentTypes(List(List("num", "list")), List(x, y))
  )
  val greaterOrEqual = NativeFunc(
    ">=",
    ls =>
      (ls(0), ls(1)) match
        case (LNum(a), LNum(b)) => List(LBool(a >= b))
        case (x, y)             => throw WrongArgumentTypes(List(List("num", "num")), List(x, y))
  )
  val less = NativeFunc(
    "<",
    ls =>
      (ls(0), ls(1)) match
        case (LNum(a), LNum(b)) => List(LBool(a < b))
        case (x, y)             => throw WrongArgumentTypes(List(List("num", "num")), List(x, y))
  )
  val equal = NativeFunc(
    "=",
    ls =>
      (ls(0), ls(1)) match
        case (LNum(a), LNum(b)) => List(LBool(a == b))
        case (x, y)             => throw WrongArgumentTypes(List(List("num", "num")), List(x, y))
  )
  val plus = NativeFunc(
    "+",
    ls =>
      (ls(0), ls(1)) match
        case (LNum(a), LNum(b)) => List(LNum(a + b))
        case (x, y)             => throw WrongArgumentTypes(List(List("num", "num")), List(x, y))
  )
  val mult = NativeFunc(
    "*",
    ls =>
      (ls(0), ls(1)) match
        case (LNum(a), LNum(b)) => List(LNum(a * b))
        case (x, y)             => throw WrongArgumentTypes(List(List("num", "num")), List(x, y))
  )
  val printlnFunc = NativeFunc(
    "println",
    ls =>
      List({
        def f(a: Any): LUnit.type =
          print(a)
          LUnit
        ls(0) match
          case LNull    => f("null")
          case LUnit    => f("()")
          case LBool(x) => f(x)
          case LNum(x)  => f(x)
          case LStr(x)  => f(x)
          case LList(ls) =>
            f("'(");
            ls.foreach { x =>
              f(x); f(", ")
            }; f(")")
            println()
            LUnit
          case Call(id, _)       => f(s"call: $id")
          case Func(id, _, _)    => f(s"func: $id")
          case NativeFunc(id, _) => f(s"native func: $id")
          case Const(id, _)      => f(s"var: $id")
          case Redef(id, _)      => f(s"var redef: $id")
          case _: If             => f("if-then-else expression")
          case _: While          => f("unsafe while statement")
      })
  )
