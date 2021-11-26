package dev.vgerasimov.lizp

def optimize(expressions: List[Expr]): List[Expr] =
  expressions
    .map({
      case f @ Func(id, params, body) =>
        body.last match
          case If(cond, Call(callId, args), elseExpr) if id == callId =>
            optimizeTailRecFunc(id, params, args, cond, elseExpr)
          case If(cond, thenExpr, Call(callId, args)) if id == callId =>
            optimizeTailRecFunc(id, params, args, cond, thenExpr)
          case _ =>
            Func(id, params, optimize(body))
      case expr => expr
    })

private def optimizeTailRecFunc(id: Id, params: List[FuncParam], args: List[Expr], cond: Expr, result: Expr): Expr =
  val ls = (params zip args).map({
    case (p, a) => {
      val id = Id(s"${p.name.v}${'$'}0")
      (
        Const(id, LUnit),
        Redef(id, a),
        Redef(p.name, Call(id))
      )
    }
  })
  Func(id, params, ls.map(_._1) ++ List(While(cond, ls.map(_._2) ++ ls.map(_._3)), result))
