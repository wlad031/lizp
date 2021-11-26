package dev.vgerasimov.lizp

private object syntax:

  extension [A](a: A)
    def asRight: Right[Nothing, A] = Right(a)
    def asLeft: Left[A, Nothing] = Left(a)

  extension [A, B](either: Either[A, B])
    def mapLeft[A1](f: A => A1): Either[A1, B] = either match
      case Left(a)  => Left(f(a))
      case Right(b) => Right(b)

  extension [A, B](listEithers: List[Either[A, B]])
    def partitionToEither: Either[List[A], List[B]] = listEithers.partition(_.isLeft) match
      case (Nil, rights) => Right(for (Right(r) <- rights) yield r)
      case (lefts, _)    => Left(for (Left(l) <- lefts) yield l)
