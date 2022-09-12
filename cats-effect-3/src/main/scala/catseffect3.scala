package avocado.instances

import avocado.AvocADO
import cats.Monad
import cats.effect.kernel.Async

object catseffect3 {
  given [F[_]: Monad: Async] : AvocADO[F] = new AvocADO[F] {
    def pure[A](a: A): F[A] = Monad[F].pure(a)
    def map[A, B](fa: F[A], f: A => B): F[B] = Monad[F].map(fa)(f)
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = Async[F].both(fa, fb)
    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }
}

