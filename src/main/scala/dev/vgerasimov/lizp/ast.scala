package dev.vgerasimov.lizp

import scala.annotation.tailrec

sealed trait Expr

case class Sym(value: String) extends Expr

case object LUnit extends Expr
case object LNull extends Expr

case class LBool(value: Boolean) extends Expr
case class LNum(value: BigDecimal) extends Expr
case class LStr(value: String) extends Expr

sealed trait LList extends Expr:
  def :+: (expr: Expr): LList = new :+:(expr, this)
  def toScala: List[Expr] = this match
    case LNil          => Nil
    case head :+: tail => head :: tail.toScala

case object LNil extends LList
case class :+:(head: Expr, tail: LList) extends LList

case class Application(ref: Sym, args: List[Expr]) extends Expr
case class Def(ref: Sym, expression: Expr) extends Expr

case class NativeFunc(func: List[Expr] => List[Expr]) extends Expr
case class Redef(expression: Expr) extends Expr
case class Lambda(params: List[FuncParam], body: List[Expr]) extends Expr

case class Include(file: String) extends Expr

case class If(condition: Expr, thenExpression: Expr, elseExpression: Expr) extends Expr

sealed trait Unsafe extends Expr
case class While(condition: Expr, sideEffects: List[Expr]) extends Unsafe

type Literal = LNull.type | LUnit.type | LBool | LNum | LStr

case class FuncParam(name: Sym, isLazy: Boolean = false)

extension (call: Application) def toString: String = s"""Call($call.ref, ${call.args.mkString(",")})"""
extension (funcParam: FuncParam) def toLazy: FuncParam = funcParam.copy(isLazy = true)
