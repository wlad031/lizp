package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*

def optimize(expressions: List[Expr]): Either[LizpError, List[Expr]] =
  expressions
    .map({
      case f @ Def(id, Lambda(params, body)) =>
        body.last match
          case If(cond, Application(callId, args), elseExpr) if id == callId =>
            optimizeTailRecFunc(id, params, args, cond, elseExpr).asRight
          case If(cond, thenExpr, Application(callId, args)) if id == callId =>
            optimizeTailRecFunc(id, params, args, Application(Sym("not"), List(cond)), thenExpr).asRight
          case _ =>
            optimize(body).map(expressions => Def(id, Lambda(params, expressions)))
      case expr => expr.asRight
    })
    .partitionToEither
    .mapLeft(LizpError.Multi(_))

private def optimizeTailRecFunc(id: Sym, params: List[FuncParam], args: List[Expr], cond: Expr, result: Expr): Expr =
  val ls = (params zip args).map({
    case (p, a) => {
      val id = Sym(s"${p.name.value}${'$'}0")
      (
        Def(id, LUnit),
        Def(id, Redef(a)),
        Def(p.name, Redef(Application(id, Nil)))
      )
    }
  })
  Def(id, Lambda(params, ls.map(_._1) ++ List(While(cond, ls.map(_._2) ++ ls.map(_._3)), result)))
