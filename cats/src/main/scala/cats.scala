package avocado.instances

import avocado.AvocADO
import _root_.cats.{Monad, Parallel}

object cats {
  given [F[_]: Monad: Parallel] : AvocADO[F] = new AvocADO[F] {
    def pure[A](a: A): F[A] = Monad[F].pure(a)
    def map[A, B](fa: F[A], f: A => B): F[B] = Monad[F].map(fa)(f)
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = Parallel.parTuple2(fa, fb)
    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }
}
