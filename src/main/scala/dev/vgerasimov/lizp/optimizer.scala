package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*

def optimize(expressions: List[Expr]): Either[LizpError, List[Expr]] =
  expressions
    .map({
      case f @ Func(id, params, body) =>
        body.last match
          case If(cond, Call(callId, args), elseExpr) if id == callId =>
            optimizeTailRecFunc(id, params, args, cond, elseExpr).asRight
          case If(cond, thenExpr, Call(callId, args)) if id == callId =>
            optimizeTailRecFunc(id, params, args, Call(Sym("not"), List(cond)), thenExpr).asRight
          case _ =>
            optimize(body).map(Func(id, params, _))
      case expr => expr.asRight
    })
    .partitionToEither
    .mapLeft(LizpError.Multi(_))

private def optimizeTailRecFunc(id: Sym, params: List[FuncParam], args: List[Expr], cond: Expr, result: Expr): Expr =
  val ls = (params zip args).map({
    case (p, a) => {
      val id = Sym(s"${p.name.value}${'$'}0")
      (
        Const(id, LUnit),
        Redef(id, a),
        Redef(p.name, Call(id))
      )
    }
  })
  Func(id, params, ls.map(_._1) ++ List(While(cond, ls.map(_._2) ++ ls.map(_._3)), result))
