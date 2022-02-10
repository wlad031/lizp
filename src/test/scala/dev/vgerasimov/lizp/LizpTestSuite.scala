package dev.vgerasimov.lizp

import dev.vgerasimov.lizp.types.*

import org.scalacheck.Gen
import org.scalacheck.Prop.*

trait Asserter[A] extends ((A, A) => Unit)

trait LizpTestSuite[A, B](underTest: A => Either[LizpError, B]) extends munit.ScalaCheckSuite:
  export LizpTestSuite.*

  given simpleAsserter[A]: Asserter[A] = (a, b) => assertEquals(a, b)

  val assertEquals = super.assertEquals

  def testGen(name: String)(testCase: (Gen[A], A => B))(using Asserter[B]) = testCase match
    case (gen, expected) =>
      test(name) {
        forAll(gen) { toPass =>
          assertRes(underTest(toPass), expected(toPass))
        }
      }

  def testOne(name: String)(testCase: (A, B))(using Asserter[B]) = testCase match
    case (toPass, expected) => test(name) { assertRes(underTest(toPass), expected) }

  def testGenError(name: String)(testCase: (Gen[A], A => LizpError)) = testCase match
    case (gen, expected) =>
      test(name) {
        forAll(gen) { toPass =>
          assertError(underTest(toPass), expected(toPass))
        }
      }

  def testOneError(name: String)(testCase: (A, LizpError)) = testCase match
    case (toPass, expected) => test(name) { assertError(underTest(toPass), expected) }

  def assertRes(obtained: Either[LizpError, B], expected: B)(using a: Asserter[B]) = obtained match
    case Left(error)   => fail(s"Failed with error:\n$error")
    case Right(actual) => a.apply(actual, expected)

  def assertError(obtained: Either[LizpError, B], expected: LizpError) = obtained match
    case Left(error)   => assertEquals(error, expected)
    case Right(actual) => fail(s"Succeed with result:\n$actual")

object LizpTestSuite:
  def one[A](value: A): Gen[A] = oneOf(value)
  def oneOf[A](values: A*): Gen[A] = Gen.oneOf(values.toSeq)

  def parseUnsafe(parser: Parser)(s: String): Expr = parser(s) match
    case Right(expr) => expr
    case Left(error) => sys.error(s"Unexpected error: $error")
