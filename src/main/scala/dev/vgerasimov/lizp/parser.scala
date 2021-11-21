package dev.vgerasimov.lizp

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.P
import dev.vgerasimov.slowparse.Parsers.*
import dev.vgerasimov.slowparse.Parsers.given

object Parser:

  private val num: P[Int] = d.+.!.map(_.toString).map(_.toInt)
  private val id: P[String] = choice(alphaNum | anyCharIn("-_+/=<>")).*.!
  private val valInt: P[ValInt] = num.map(ValInt(_))
  private val valDouble: P[ValDouble] = (num ~ P(".") ~ num).map { case (a, b) => a + 0.1 * b }.map(ValDouble(_))
  private val valString: P[ValString] = (P("\"") ~ until(P("\"")).! ~ P("\"")).map(ValString(_))
  private val valBoolean: P[ValBoolean] = (P("true").! | P("false").!).map(_.toBoolean).map(ValBoolean(_))
  private val valUnit: P[ValUnit.type] = P("()").map(_ => ValUnit)
  private val definition: P[Def] =
    val params: P[List[String]] = P('(') ~ (wss ~ id.rep(sep = ws1)).?.map(_.getOrElse(Nil)) ~ P(')')
    P(P('(') ~~ P("def") ~-~ id ~ (ws1 ~ params).? ~ (ws1 ~ expr).+ ~~ P(')')).map { case (name, p, e) =>
      Def(name, p.getOrElse(Nil), ls => e.map(x => () => x))
    }
  private val call: P[Call] =
    P((P('(') ~~ id ~ (ws ~ expr.rep(sep = ws1)).? ~~ P(')')).map { case (name, e) =>
      Call(name, e.getOrElse(Nil).map(x => () => x))
    })
  private val expr: P[Expr] =
    P(choice(valInt, valDouble, valString, valBoolean, valUnit, definition, call))

  def apply(string: String): ParsingError | List[Expr] = (wss ~ expr.rep(sep = ws1) ~~ end)(string) match
    case POut.Success(v, _, _, _) => v
    case f @ POut.Failure(message, _) =>
      println(f)
      ParsingError(message)

case class ParsingError(message: String)
