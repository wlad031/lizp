package dev.vgerasimov.lizp

sealed trait Expr

sealed trait Val extends Expr

case object ValUnit extends Val
case class ValInt(v: Int) extends Val
case class ValString(v: String) extends Val
case class ValBoolean(v: Boolean) extends Val
case class ValDouble(v: Double) extends Val

type LazyExpr = () => Expr

case class Call(
  name: String,
  args: List[LazyExpr]
) extends Expr

case class Def(
  name: String,
  params: List[String],
  func: List[LazyExpr] => List[LazyExpr]
) extends Expr
