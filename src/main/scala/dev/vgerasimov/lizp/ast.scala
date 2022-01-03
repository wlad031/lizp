package dev.vgerasimov.lizp

sealed trait Expr

case class Sym(value: String) extends Expr

case object LUnit extends Expr
case object LNull extends Expr

case class LBool(value: Boolean) extends Expr
case class LNum(value: BigDecimal) extends Expr
case class LStr(value: String) extends Expr

case class LList(value: List[Expr]) extends Expr

case class Call(ref: Sym, args: List[Expr] = Nil) extends Expr

case class NativeFunc(name: Sym, func: List[Expr] => List[Expr]) extends Expr
case class Func(name: Sym, params: List[FuncParam], body: List[Expr]) extends Expr
case class Const(name: Sym, expression: Expr) extends Expr
case class Redef(ref: Sym, expression: Expr) extends Expr
case class Lambda(params: List[FuncParam], body: List[Expr]) extends Expr

case class Include(file: String) extends Expr

case class If(condition: Expr, thenExpression: Expr, elseExpression: Expr) extends Expr

sealed trait Unsafe extends Expr
case class While(condition: Expr, sideEffects: List[Expr]) extends Unsafe

type Literal = LNull.type | LUnit.type | LBool | LNum | LStr
type Definition = Const | Func | NativeFunc | Redef

case class FuncParam(name: Sym, isLazy: Boolean = false)

extension (call: Call) def toString: String = s"""Call($call.ref, ${call.args.mkString(",")})"""
extension (func: Func) def toString: String = s"Func($func.name, $func.params, $func.body)"
extension (funcParam: FuncParam) def toLazy: FuncParam = funcParam.copy(isLazy = true)
