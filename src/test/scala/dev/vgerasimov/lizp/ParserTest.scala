package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.types.*

import org.scalacheck.Gen
import org.scalacheck.Prop.*

class ParserTest extends LizpTestSuite(Parser()):

  testOne("empty string -> Nil") { "" -> Nil }
  testGen("spaces only -> Nil") { Gen.stringOf(Gen.oneOf(" \r\t\n".toCharArray)) -> (_ => Nil) }
  testGen("num string -> Num") { Gen.numStr.filter(_.nonEmpty) -> (s => Num(BigDecimal(s))) }
  testGen("(num string) -> (Num)") {
    Gen.numStr.filter(_.nonEmpty).map(v => s"($v)") ->
    (s => List(Num(BigDecimal(s.replace(")", "").replace("(", "")))))
  }
  testGen("boolean -> Bool") { oneOf("true", "false") -> (s => Bool(s.toBoolean)) }
  testGen("quoted string -> Str") {
    Gen.asciiStr.filter(!_.contains("\"")).map(x => s""""$x"""") ->
    (s => Str(s.stripPrefix("\"").stripSuffix("\"")))
  }
  testOne("empty string and trailing comment -> Nil") { "; hello" -> Nil }
  testOne("""(1 "hello") and trailing comment -> List""") { """(1 "hello") ; comment""" -> List(Num(1), Str("hello")) }
  testOne("""different comments""") {
    """|;start
       |(1 "hello") ; comment 1.1
       |(1 "hello" "world") ; comment 1.2
       |;comment 2 ("ignored")
       |(foo x)
       |; lol
       |; the second
       |(var)
       |;end
    """.stripMargin -> List(
      List(Num(1), Str("hello")),
      List(Num(1), Str("hello"), Str("world")),
      List(Sym("foo"), Sym("x")),
      List(Sym("var"))
    )
  }
