package avocado

import macros.*

//TODO(kπ) eventually docs
inline def ado[F[_], A](inline comp: F[A])(using ap: AvocADO[F]): F[A] =
  ${ macros.adoImpl('comp, 'ap) }

trait AvocADO[F[_]] {
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A], f: A => B): F[B]
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
}
