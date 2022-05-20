package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.syntax.*

trait Repl[A] extends (Repl.Handler[A] => A => Unit)

object Repl:
  private lazy val impl = SimpleRepl

  def apply(): Repl[Interpreter.Scopes] = impl

  trait Handler[A] extends ((String, A) => (String, A))

  def lizpHandler(interpreter: Interpreter, context: Interpreter.Context): Handler[Interpreter.Scopes] = (cmd, ctx) =>
    (for {
      parsed <- context.parser(cmd)
      evaluated <- {
        given Interpreter.Context = context
        interpreter(parsed, ctx)
      }
    } yield evaluated).map(p => (p._1.toString, p._2)).mapLeft(e => (e.toString, ctx)).unwrap

private object SimpleRepl extends Repl[Interpreter.Scopes]:
  import scala.io.StdIn

  override def apply(handler: Repl.Handler[Interpreter.Scopes]): (Interpreter.Scopes => Unit) = ctx =>
    println("Welcome to the Lizp REPL")
    var c = ctx
    while (true) {
      print("> ")
      var index = 0
      var readChar = java.lang.System.in.read()
      var read = ""
      while (readChar != 13 && readChar != 10) {
        java.lang.System.out.print(readChar)
        read = read + readChar.toChar
        readChar = java.lang.System.in.read().toChar
      }
      val eval = handler(read, c)
      println(eval._1)
      c = eval._2
    }
