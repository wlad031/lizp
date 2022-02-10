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
