package avocado

import macros.*

/**
  * Transforms the provided for-comprehension to it's parallel version.
  * Example usage:
  * ```scala
  * ado {
  *   for {
  *     a <- doStuff1
  *     b <- doStuff2(a)
  *     c <- doStuff3
  *     d <- doStuff4(a)
  *   } yield combine(a, b, c, d)
  * }
  * ```
  *
  * The above code will be transformed to code essentially equivalent to:
  * ```scala
  * for {
  *   a <- doStuff1
  *   (b, c, d) <- doStuff2(a).zip(doStuff3).zip(doStuff4(a))
  * } yield combine(a, b, c, d)
  * ```
  *
  * The transformed code will use the provided implicit [[avocado.AvocADO]]
  * instance for method calls such as `map`, `flatMap` and `zip`. Potential for
  * parallelism is introduced in places where `zip` calls are used. So in order
  * to utilize this method in a sensible way, [[avocado.AvocADO.zip]] should
  * initialize parallel calls. Though this method should also be safe for
  * sequential operations.
  */
inline def ado[F[_], A](inline comp: F[A])(using ap: AvocADO[F]): F[A] =
  ${ macros.adoImpl[F, A]('comp, 'ap) }

trait AvocADO[F[_]] {
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A], f: A => B): F[B]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
}
