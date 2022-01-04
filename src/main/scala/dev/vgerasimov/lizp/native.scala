package dev.vgerasimov.lizp

private[lizp] object native:
  given Conversion[String, Sym] = Sym(_)
  given Conversion[Sym, FuncParam] = FuncParam(_, isLazy = false)
  given Conversion[String, FuncParam] with
    def apply(string: String): FuncParam = FuncParam(Sym(string), isLazy = false)

  import ExecutionError.*

  lazy val all: List[Def] = List(
    nil,
    list,
    head,
    tail,
    not,
    greaterOrEqual,
    lessOrEqual,
    greater,
    less,
    equal,
    notEqual,
    plus,
    mult,
    minus,
    divide,
    printlnFunc
  )

  private def in1out1(name: String, func: PartialFunction[Expr, Expr]): Def = Def(
    name,
    NativeFunc(ls => {
      val arg = ls(0)
      if (func.isDefinedAt(arg)) List(func(arg))
      else throw WrongArgumentTypes(s"""Function "$name" is not defined for the argument "$arg"""") // FIXME: error args
    })
  )

  private def in2out1(name: String, func: PartialFunction[(Expr, Expr), Expr]): Def = Def(
    name,
    NativeFunc(ls => {
      val arg = (ls(0), ls(1))
      if (func.isDefinedAt(arg)) List(func(arg))
      else throw WrongArgumentTypes(s"""Function "$name" is not defined for the argument "$arg"""") // FIXME: error args
    })
  )

  val nil = Def("nil", LNil)

// format: off
  val head = in1out1("head", { case head :+: tail => head })
  val tail = in1out1("tail", { case head :+: tail => tail })
  val list = Def("list", NativeFunc(ls => List(ls.foldRight[LList](LNil)(_ :+: _))))
  val not = in1out1("not", { case LBool(a) => LBool(!a) })

  val greaterOrEqual = in2out1(">=",  { case (LNum(a), LNum(b)) => LBool(a >= b) })
  val lessOrEqual    = in2out1("<=",  { case (LNum(a), LNum(b)) => LBool(a <= b) })
  val greater        = in2out1(">",   { case (LNum(a), LNum(b)) => LBool(a > b) })
  val less           = in2out1("<",   { case (LNum(a), LNum(b)) => LBool(a < b) })
  val equal          = in2out1("=",   { case (a, b) => LBool(a.equals(b)) })
  val notEqual       = in2out1("!=",  { case (a, b) => LBool(!a.equals(b)) })
  val plus           = in2out1("+",   { case (LNum(a), LNum(b)) => LNum(a + b)
                                        case (LStr(a), LStr(b)) => LStr(a + b) })
  val mult           = in2out1("*",   { case (LNum(a), LNum(b)) => LNum(a * b) })
  val minus          = in2out1("-",   { case (LNum(a), LNum(b)) => LNum(a - b) })
  val divide         = in2out1("/",   { case (LNum(a), LNum(b)) => LNum(a / b) })
// format: on
  val printlnFunc = Def(
    "println",
    NativeFunc(ls =>
      List({
        def f(a: Any): LUnit.type =
          println(a)
          LUnit
        ls(0) match
          case LNull    => f("null")
          case LUnit    => f("()")
          case LBool(x) => f(x)
          case LNum(x)  => f(x)
          case LStr(x)  => f(x)
          case LNil     => f("nil")
//          case LList(ls) =>
//            f("'(");
//            ls.foreach { x =>
//              f(x); f(", ")
//            }; f(")")
//            println()
//            LUnit
          case Application(id, _) => f(s"call: $id")
          case Def(id, _)         => f(s"ref: $id")
          case _: If              => f("if-then-else expression")
          case _: While           => f("unsafe while statement")
      })
    )
  )
