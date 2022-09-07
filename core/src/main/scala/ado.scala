package avocado

import macros.*

//TODO(kπ) eventually docs
inline def ado[F[_], A](inline comp: F[A])(using ap: Applicative[F]): F[A] =
  ${ macros.adoImpl('comp, 'ap) }

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}
