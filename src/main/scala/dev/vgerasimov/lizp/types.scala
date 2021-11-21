package dev.vgerasimov.lizp

sealed trait Expr

case object LUnit extends Expr
case object LNull extends Expr

case class LInt(v: Int) extends Expr
case class LStr(v: String) extends Expr
case class LBool(v: Boolean) extends Expr
case class LDouble(v: Double) extends Expr

type LazyExpr = () => Expr

case class Id(v: String)

case class Lambda(
  params: List[Id], 
  func: List[LazyExpr] => LazyExpr
) extends Expr

case class Call(
  name: Id,
  args: List[LazyExpr]
) extends Expr

case class Func(
  name: Id,
  params: List[Id],
  func: List[LazyExpr] => List[LazyExpr]
) extends Expr

case class Const(
  name: Id,
  value: List[LazyExpr]
) extends Expr
