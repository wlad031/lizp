package dev.vgerasimov.lizp
package types

import dev.vgerasimov.lizp.syntax.*
import scala.annotation.unchecked.uncheckedVariance

sealed trait Expr

sealed trait Keyword extends Expr
object Keyword:
  case object Fn extends Keyword
  case object Def extends Keyword
  case object List extends Keyword
  case object Eval extends Keyword
  case object If extends Keyword
  case object Macro extends Keyword

case class Sym(value: String) extends Expr:
  override def toString: String = s"'$value" // TODO: get rid of it

object Sym:
  val Def = Sym("def")
  val Fn = Sym("fn")
  val Macro = Sym("macro")
  val List = Sym("list")
  val Eval = Sym("eval")
  val If = Sym("if")

sealed trait Param:
  def sym: Sym

object Param:
  def apply(sym: Sym): Param =
    // TODO: refactor this
    if (sym.value.startsWith("...")) Param.VarArg(Param(Sym(sym.value.substring(3))))
    else if (!sym.value.startsWith(">")) Param.ByValue(sym)
    else Param.ByName(Sym(sym.value.replace(">", "")))

  case class ByValue(override val sym: Sym) extends Param
  case class ByName(override val sym: Sym) extends Param
  case class VarArg(param: Param) extends Param:
    override val sym = param.sym

sealed trait Atom extends Expr
case class Str(value: String) extends Atom:
  override def toString: String = s""""$value""""
case class Bool(value: Boolean) extends Atom:
  override def toString: String = s"""$value"""
case class Num(value: BigDecimal) extends Atom:
  override def toString: String = s"""$value"""

sealed trait List[+E <: Expr] extends Expr:
  @uncheckedVariance
  def :: (expr: Expr): List[Expr] = new ::(expr, this)
  override def toString: String = this.asScala.mkString("[", ", ", "]")

case object Nil extends List[Nothing]
case class ::[E <: Expr](head: E, tail: List[E]) extends List[E]

case class Apply(target: Expr, args: List[Expr]) extends Expr

case class Deref(sym: Sym) extends Expr
case class Note(message: String) extends Expr:
  override def toString: String = message
case class Fn(params: List[Sym], body: Expr) extends Expr
case class Native[S, C](fn: Native.Fn[S, C]) extends Expr:
  override def toString: String = "<native>" // TODO: get rid of it
object Native:
  trait Fn[S, C]:
    def apply(expressions: List[Expr], scopes: S, ctx: C): Either[Native.Error, (Expr, S)]
  case class Error(message: String) extends LizpError
case class Macro(params: List[Sym], expr: Expr) extends Expr

case class EvalOnDeref(expr: Expr) extends Expr

sealed trait Unsafe extends Expr
object Unsafe:
  case class WhileLoop() extends Unsafe

object List:
  def sym(elems: Sym*): List[Sym] = elems.toList.asLizp
  def apply[E <: Expr](elems: E*): List[Expr] = elems.toList.asLizp
