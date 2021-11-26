package dev.vgerasimov.lizp

case class Id(v: String)

sealed trait Expr

case object LUnit extends Expr
case object LNull extends Expr

case class LBool(v: Boolean) extends Expr
case class LNum(v: BigDecimal) extends Expr
case class LStr(v: String) extends Expr

case class EList(v: List[Expr]) extends Expr

case class Call(
  id: Id,
  args: List[Expr] = Nil
) extends Expr:
  override def toString: String = s"""Call($id, ${args.mkString(",")})"""

case class FuncParam(name: Id, isLazy: Boolean = false)

extension (funcParam: FuncParam) def toLazy: FuncParam = funcParam.copy(isLazy = true)

case class NativeFunc(id: Id, func: List[Expr] => List[Expr]) extends Expr

case class Func(
  id: Id,
  params: List[FuncParam],
  body: List[Expr]
) extends Expr:
  override def toString: String = s"Func($id, $params, $body)"

case class Const(
  id: Id,
  exprs: Expr
) extends Expr

case class Redef(
  id: Id,
  expr: Expr
) extends Expr

case class If(
  condition: Expr,
  thenExpr: Expr,
  elseExpr: Expr
) extends Expr

sealed trait Unsafe extends Expr

case class While(
  condition: Expr,
  sideEffects: List[Expr]
) extends Unsafe

type Literal = LNull.type | LUnit.type | LBool | LNum | LStr
type Definition = Const | Func | NativeFunc | Redef
