package dev.vgerasimov.lizp

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.P
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given

object Parser:
  private val id: P[Id] = choice(alphaNum | anyCharIn("-_+*#@!?^\\|;:.,~/=<>%$")).*.!.map(Id(_))

  private val lUnit: P[LUnit.type] = P("()").map(_ => LUnit)
  private val lNull: P[LNull.type] = P("null").map(_ => LNull)
  private val lBool: P[LBool] = (P("true").! | P("false").!).map(_.toBoolean).map(LBool(_))

  private val num: P[Int] = d.+.!.map(_.toString).map(_.toInt)
  private val lInt: P[LInt] = num.map(LInt(_))
  private val lDouble: P[LDouble] = (num ~ P(".") ~ num).map { case (a, b) => a + 0.1 * b }.map(LDouble(_))

  private val lStr: P[LStr] = (P("\"") ~ until(P("\"")).! ~ P("\"")).map(LStr(_))

  private val eList: P[EList] = 
    P(P('(') ~~ P("list") ~ (ws1 ~ expr).+ ~~ P(')'))
      .map(exprs => EList(exprs))

  private val func: P[Func] =
    val params: P[List[Id]] = P('(') ~ (wss ~ id.rep(sep = ws1)).?.map(_.getOrElse(Nil)) ~ P(')')
    P(P('(') ~~ P("def") ~-~ id ~-~ params ~ (ws1 ~ expr).+ ~~ P(')'))
      .map { case (name, p, exprs) => Func(name, p, ls => exprs.map(expr => () => expr)) }

  private val const: P[Const] =
    P(P('(') ~~ P("val") ~-~ id ~ (ws1 ~ expr).+ ~~ P(')'))
      .map { case (name, exprs) => Const(name, exprs.map(expr => () => expr)) }

  private val call: P[Call] =
    P((P('(') ~~ id ~ (ws ~ expr.rep(sep = ws1)).? ~~ P(')')).map { case (name, e) =>
      Call(name, e.getOrElse(Nil).map(x => () => x))
    })

  private val expr: P[Expr] =
    P(choice(lNull, lUnit, lBool, lInt, lDouble, lStr, eList, func, const, call))

  def apply(string: String): ParsingError | List[Expr] = (wss ~ expr.rep(sep = ws1) ~~ end)(string) match
    case POut.Success(v, _, _, _) => v
    case f @ POut.Failure(message, _) =>
      ParsingError(message)

case class ParsingError(message: String)
