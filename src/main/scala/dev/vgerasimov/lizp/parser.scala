package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*
import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.Parsers.{ *, given }

import scala.util.Random

def parse(string: String): Either[ParsingError, List[Expr]] = Parser.apply(string)

def expand(expressions: List[Expr]): Either[LizpError, List[Expr]] =
  expressions.map(expand).partitionToEither.mapLeft(LizpError.Multi.apply)

//noinspection ScalaUnnecessaryParentheses
private def expand(expression: Expr): Either[LizpError, Expr] =
  expression match
    case literal: Literal                           => literal.asRight
    case ref: Sym                                   => Call(ref).asRight
    case LList(Sym("native") :: (ref: Sym) :: Nil)  => NativeFunc(ref, null).asRight
    case LList(Sym("include") :: LStr(file) :: Nil) => Include(file).asRight
    case LList(Sym("def") :: (name: Sym) :: LList(params) :: body) =>
      body
        .map(expand)
        .partitionToEither
        .map(Func(name, params.map(_.asInstanceOf[Sym]).map(FuncParam(_)), _))
        .mapLeft(LizpError.Multi.apply)
    case LList(Sym("val") :: (name: Sym) :: body :: Nil) =>
      expand(body).map(Const(name, _))
    case LList(Sym("lambda") :: LList(params) :: body) =>
      body
        .map(expand)
        .partitionToEither
        .map(Lambda(params.map(_.asInstanceOf[Sym]).map(FuncParam(_)), _))
        .mapLeft(LizpError.Multi.apply)
    case LList(Sym("if") :: condition :: thenExpression :: elseExpression :: Nil) =>
      for {
        cond     <- expand(condition)
        thenExpr <- expand(thenExpression)
        elseExpr <- expand(elseExpression)
      } yield If(cond, thenExpr, elseExpr)
    case LList((ref: Sym) :: args) =>
      args
        .map(expand)
        .partitionToEither
        .map(Call(ref, _))
        .mapLeft(LizpError.Multi.apply)
    case list: LList => list.asRight
    case expression  => ExpansionError(s"Unexpected expression: $expression").asLeft

private object Parser:
  private val sym: P[Sym] =
    ((alphaNum | anyFrom("-_+*#@!?^\\|;:.,~/=<>%$")).! ~ until(anyFrom(" \t\r\n()[]{}")).!)
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

  //noinspection ForwardReference
  private val lList: P[LList] =
    P(
      (anyFrom("([{") ~ ws0).!.flatMap(bracket =>
        expr.rep(sep = ws1) ~~ P(bracket match {
          case "(" => ")"
          case "[" => "]"
          case "{" => "}"
        })
      ).map(LList(_))
    )

  private val expr: P[Expr] =
    P(choice(lNull, lBool, lNum, lStr, sym, lList))

  def apply(string: String): Either[ParsingError, List[Expr]] = (ws0 ~ expr.rep(sep = ws1) ~~ end)(string) match
    case POut.Success(v, _, _, _)     => v.asRight
    case f @ POut.Failure(message, _) => ParsingError(message).asLeft

case class ExpansionError(message: String) extends LizpError
case class ParsingError(message: String) extends LizpError
