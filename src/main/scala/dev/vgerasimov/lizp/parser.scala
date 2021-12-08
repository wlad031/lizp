package dev.vgerasimov.lizp

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.P
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given

import dev.vgerasimov.lizp.syntax.*

def parse(string: String): Either[ParsingError, List[Expr]] = Parser.apply(string)

def expand(expressions: List[Expr]): Either[LizpError, List[Expr]] =
  expressions.map(expand).partitionToEither.mapLeft(LizpError.Multi(_))

private def expand(expression: Expr): Either[LizpError, Expr] =
  expression match
    case literal: Literal => literal.asRight
    case LList(Sym("def") :: LList(Sym(name) :: params) :: body) =>
      body
        .map(expand)
        .partitionToEither
        .map(Func(Sym(name), params.map(_.asInstanceOf[Sym]).map(FuncParam(_)), _))
        .mapLeft(LizpError.Multi(_))
    case LList(Sym("val") :: Sym(name) :: body :: Nil) =>
      expand(body).map(Const(Sym(name), _))
    case LList(Sym("if") :: condition :: thenExpression :: elseExpression :: Nil) =>
      for {
        cond     <- expand(condition)
        thenExpr <- expand(thenExpression)
        elseExpr <- expand(elseExpression)
      } yield If(cond, thenExpr, elseExpr)
    case LList(Sym(ref) :: args) =>
      args
        .map(expand)
        .partitionToEither
        .map(Call(Sym(ref), _))
        .mapLeft(LizpError.Multi(_))
    case list: LList => list.asRight
    case expression  => ExpansionError(s"Unexpected expression: $expression").asLeft

private object Parser:
  private val sym: P[Sym] =
    ((alphaNum | anyFrom("-_+*#@!?^\\|;:.,~/=<>%$")).! ~ until(anyFrom(" \t\r\n()")).!)
      .map({ case (head, tail) => Sym(head + tail) })

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
    P(P("(") ~~ expr.rep(sep = ws1) ~~ P(')')).map(LList(_))

  private val expr: P[Expr] =
    P(choice(lNull, lBool, lNum, lStr, sym, lList))

  def apply(string: String): Either[ParsingError, List[Expr]] = (ws0 ~ expr.rep(sep = ws1) ~~ end)(string) match
    case POut.Success(v, _, _, _)     => v.asRight
    case f @ POut.Failure(message, _) => ParsingError(message).asLeft

case class ExpansionError(message: String) extends LizpError
case class ParsingError(message: String) extends LizpError
