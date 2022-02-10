package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*
import dev.vgerasimov.lizp.types.*

/** Parses given string into [[Expr]] or [[Parser.Error]] if string is not a valid expression. */
trait Parser extends (String => Either[Parser.Error, Expr])

/** Contains parser-related types and functions. */
object Parser:

  private lazy val impl = SlowparseParser

  /** Returns implementation of [[Parser]]. */
  def apply(): Parser = impl

  /** Represents a parsing error. */
  case class Error(message: String) extends LizpError

/** [[Parser]] implementation using [[https://github.com/wlad031/slowparse Slowparse]] library. */
//noinspection ForwardReference
object SlowparseParser extends Parser:

  import dev.vgerasimov.slowparse.*
  import dev.vgerasimov.slowparse.Parsers.{ *, given }

  override def apply(string: String): Either[Parser.Error, Expr] = exprs(string) match
    case POut.Success(result, _, _, _) => result.asRight
    case POut.Failure(message, _)      => Parser.Error(message).asLeft

  private val comment: P[Unit] = P(";") ~ until(eol)

  private val expr: P[Expr] = P(atom | sym | list)
  private val exprs: P[Expr] =
    import renamings.PrefixLizpTypes.*
    (ws0 ~ expr.rep(sep = Some(ws1)) ~~ end).map({
      case Nil      => LizpNil
      case x :: Nil => x
      case list     => list.asLizp
    })

  private val atom: P[Atom] = P(choice(bool, num, str))

  private val sym: P[Sym] =
    ((alphaNum | anyFrom("'-_+*#@!?^\\|;:.,~/=<>%$")).! ~ until(anyFrom(" \t\r\n()[]{}")).!)
      .map({ case (head, tail) => Sym(head + tail) })

  private val bool: P[Bool] = (P("true").! | P("false").!).map(_.toBoolean).map(Bool.apply)

  private val num: P[Num] =
    val digits: P[String] = d.+.!.map(_.toString)
    val sign: P[String] = (P('-') | P('+')).!
    (sign.? ~ digits ~ (P('.') ~ digits).?)
      .map({
        case (Some("-"), n1, Some(n2)) => BigDecimal(s"-$n1.$n2")
        case (Some("-"), n1, None)     => BigDecimal(s"$n1")
        case (_, n1, Some(n2))         => BigDecimal(s"$n1.$n2")
        case (_, n1, None)             => BigDecimal(s"$n1")
      })
      .map(Num.apply)

  private val str: P[Str] = (P("\"") ~ until(P("\"")).! ~ P("\"")).map(Str.apply)

  private val list: P[List[Expr]] =
    P(
      (anyFrom("([{") ~ ws0).!.flatMap(bracket =>
        expr.rep(sep = Some(ws1)) ~~ P(bracket match {
          case "(" => ")"
          case "[" => "]"
          case "{" => "}"
        })
      ).map(_.asLizp)
    )
