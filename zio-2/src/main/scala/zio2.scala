package avocado.zio2

import avocado.Applicative
import zio.ZIO

given [E, R]: Applicative[[X] =>> ZIO[E, R, X]] = new Applicative[[X] =>> ZIO[E, R, X]] {
  def pure[A](a: A): ZIO[E, R, A] = ZIO.succeed(a)
  def zip[A, B](fa: ZIO[E, R, A], fb: ZIO[E, R, B]): ZIO[E, R, (A, B)] = fa.zipPar(fb)
}
