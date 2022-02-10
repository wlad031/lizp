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
    case literal: Literal                       => literal.asRight
    case ref: Sym                               => Application(ref, Nil).asRight
    case Sym("native") :+: (ref: Sym) :+: LNil  => Def(ref, NativeFunc(null)).asRight
    case Sym("include") :+: LStr(file) :+: LNil => Include(file).asRight
    case Sym("def") :+: (name: Sym) :+: (params: LList) :+: body =>
      body.toScala
        .map(expand)
        .partitionToEither
        .map(bodyExpressions =>
          Def(name, Lambda(params.toScala.map(_.asInstanceOf[Sym]).map(FuncParam(_)), bodyExpressions))
        )
        .mapLeft(LizpError.Multi.apply)
    case Sym("val") :+: (name: Sym) :+: body :+: LNil =>
      expand(body).map(Def(name, _))
    case Sym("lambda") :+: (params: LList) :+: body =>
      body.toScala
        .map(expand)
        .partitionToEither
        .map(Lambda(params.toScala.map(_.asInstanceOf[Sym]).map(FuncParam(_)), _))
        .mapLeft(LizpError.Multi.apply)
    case Sym("if") :+: condition :+: thenExpression :+: elseExpression :+: LNil =>
      for {
        cond     <- expand(condition)
        thenExpr <- expand(thenExpression)
        elseExpr <- expand(elseExpression)
      } yield If(cond, thenExpr, elseExpr)
    case (ref: Sym) :+: args =>
      args.toScala
        .map(expand)
        .partitionToEither
        .map(Application(ref, _))
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
        expr.rep(sep = Some(ws1)) ~~ P(bracket match {
          case "(" => ")"
          case "[" => "]"
          case "{" => "}"
        })
      ).map(_.foldRight[LList](LNil)(_ :+: _))
    )

  private val expr: P[Expr] =
    P(choice(lNull, lBool, lNum, lStr, sym, lList))

  def apply(string: String): Either[ParsingError, List[Expr]] = (ws0 ~ expr.rep(sep = Some(ws1)) ~~ end)(string) match
    case POut.Success(v, _, _, _)     => v.asRight
    case f @ POut.Failure(message, _) => ParsingError(message).asLeft

case class ExpansionError(message: String) extends LizpError
case class ParsingError(message: String) extends LizpError
