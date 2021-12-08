package dev.vgerasimov.lizp

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.P
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given

object Parser:
  private val sym: P[Sym] = choice(alphaNum | anyFrom("-_+*#@!?^\\|;:.,~/=<>%$")).*.!.map(Sym(_))

  private val lUnit: P[LUnit.type] = P("()").map(_ => LUnit)
  private val lNull: P[LNull.type] = P("null").map(_ => LNull)
  private val lBool: P[LBool] = (P("true").! | P("false").!).map(_.toBoolean).map(LBool(_))

  private val lNum: P[LNum] =
    val num: P[String] = d.+.!.map(_.toString)
    val sign: P[String] = (P('-') | P('+')).!
    (sign.? ~ num ~ (P('.') ~ num).?).map({
      case (Some("-"), n1, Some(n2)) => LNum(BigDecimal(s"-$n1.$n2"))
      case (Some("-"), n1, None)     => LNum(BigDecimal(s"$n1"))
      case (_, n1, Some(n2))         => LNum(BigDecimal(s"$n1.$n2"))
      case (_, n1, None)             => LNum(BigDecimal(s"$n1"))
    })

  private val lStr: P[LStr] = (P("\"") ~ until(P("\"")).! ~ P("\"")).map(LStr(_))

  private val lList: P[LList] =
    P(P("'(") ~~ expr.rep(sep = ws1) ~~ P(')')).map(LList(_))

  private val fElse: P[If] =
    P(P('(') ~~ P("if") ~-~ expr ~-~ expr ~-~ expr ~~ P(')'))
      .map({ case (cond, thenExpr, elseExpr) => If(cond, thenExpr, elseExpr) })

  private val func: P[Func] =
    val params: P[List[FuncParam]] =
      P('(')
      ~ (ws0 ~ (P("=>").?.map(_.isDefined) ~ sym)
        .rep(sep = ws1)
        .map(_.map({ case (isLazy, id) => FuncParam(id, isLazy) }))).?.map(_.getOrElse(Nil))
      ~ P(')')
    P(P('(') ~~ P("def") ~-~ sym ~-~ params ~ (ws1 ~ expr).+ ~~ P(')'))
      .map({ case (name, p, exprs) => Func(name, p, exprs) })

  private val const: P[Const] =
    P(P('(') ~~ P("val") ~-~ sym ~-~ expr ~~ P(')'))
      .map({ case (name, expr) => Const(name, expr) })

  private val call: P[Call] =
    P((P('(') ~~ sym ~ (ws ~ expr.rep(sep = ws1)).? ~~ P(')')).map({ case (name, e) =>
      Call(name, e.getOrElse(Nil))
    }))

  private val expr: P[Expr] =
    P(choice(lNull, lUnit, lBool, lNum, lStr, lList, fElse, func, const, call))

  def apply(string: String): ParsingError | List[Expr] = (ws0 ~ expr.rep(sep = ws1) ~~ end)(string) match
    case POut.Success(v, _, _, _) => v
    case f @ POut.Failure(message, _) =>
      ParsingError(message)

case class ParsingError(message: String)
