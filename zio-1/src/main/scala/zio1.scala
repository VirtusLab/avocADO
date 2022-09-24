package avocado.instances

import avocado.AvocADO
import zio.ZIO

object zio1 {
  given [E, R]: AvocADO[[X] =>> ZIO[E, R, X]] = new AvocADO[[X] =>> ZIO[E, R, X]] {
    def pure[A](a: A): ZIO[E, R, A] = ZIO.succeed(a)
    def map[A, B](fa: ZIO[E, R, A], f: A => B): ZIO[E, R, B] = fa.map(f)
    def zip[A, B](fa: ZIO[E, R, A], fb: ZIO[E, R, B]): ZIO[E, R, (A, B)] = fa.zipPar(fb)
    def flatMap[A, B](fa: ZIO[E, R, A], f: A => ZIO[E, R, B]): ZIO[E, R, B] = fa.flatMap(f)
  }
}
