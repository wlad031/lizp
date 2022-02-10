package dev.vgerasimov.lizp

/** Base Lizp error. All other errors are subtypes of this one. */
trait LizpError

/** Contains aliases for similarly named Scala and Lizp types. */
object renamings:

  /** Adds "Lizp" prefix to some Lizp types */
  object PrefixLizpTypes:
    export dev.vgerasimov.lizp.types.{ :: as LizpCons }
    export dev.vgerasimov.lizp.types.{ Nil as LizpNil }
    export dev.vgerasimov.lizp.types.{ List as LizpList }
    export scala.collection.immutable.{ List, ::, Nil, Map }

  /** Adds "Scala" prefix to some Scala types. */
  object PrefixScalaTypes:
    export scala.collection.immutable.{ List as ScalaList }
    export scala.collection.immutable.{ :: as ScalaCons }
    export scala.collection.immutable.{ Nil as ScalaNil }
    export scala.collection.immutable.{ Map as ScalaMap }
    export dev.vgerasimov.lizp.types.{ List, ::, Nil }

/** Contains useful extensions for Scala and Lizp types. */
object syntax:

  import dev.vgerasimov.lizp.types.Expr
  import renamings.PrefixLizpTypes.*

  extension [A](a: A)
    /** Wraps value into [[Right]]. */
    def asRight: Right[Nothing, A] = Right(a)

    /** Wraps value into [[Left]]. */
    def asLeft: Left[A, Nothing] = Left(a)

  extension [A, B](either: Either[A, B])
    def mapLeft[A1](f: A => A1): Either[A1, B] = either match
      case Left(a)  => Left(f(a))
      case Right(b) => Right(b)

  extension [A, B](listEithers: List[Either[A, B]])
    def partitionToEither: Either[List[A], List[B]] = listEithers.partition(_.isLeft) match
      case (Nil, rights) => Right(rights.map(_.asInstanceOf[Right[?, B]].value))
      case (lefts, _)    => Left(lefts.map(_.asInstanceOf[Left[A, ?]].value))

  extension [E <: Expr](list: List[E])
    def asLizp: LizpList[E] = list.foldRight[LizpList[E]](LizpNil)((x, acc) => new LizpCons(x, acc))

  extension [E <: Expr](list: LizpList[E])
    def asScala: List[E] =
      @scala.annotation.tailrec
      def iter(ls: LizpList[E], res: List[E]): List[E] = ls match
        case LizpNil         => res
        case LizpCons(x, xs) => iter(xs, res.appended(x))
      iter(list, Nil)
    def size: Int =
      @scala.annotation.tailrec
      def iter(ls: LizpList[E], res: Int): Int = ls match
        case LizpNil         => res
        case LizpCons(_, xs) => iter(xs, res + 1)
      iter(list, 0)
    def last: E =
      @scala.annotation.tailrec
      def iter(ls: LizpList[E], res: E): E = ls match
        case LizpNil              => res
        case LizpCons(head, tail) => iter(tail, head)
      iter(list, null.asInstanceOf[E])
