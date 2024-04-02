package avocado.instances

import avocado.AvocADO
import zio.query.ZQuery

object zioquery {
  given [E, R]: AvocADO[[X] =>> ZQuery[R, E, X]] = new AvocADO[[X] =>> ZQuery[R, E, X]] {
    def pure[A](a: A): ZQuery[R, E, A] = ZQuery.succeed(a)
    def map[A, B](fa: ZQuery[R, E, A], f: A => B): ZQuery[R, E, B] = fa.map(f)
    def zip[A, B](fa: ZQuery[R, E, A], fb: ZQuery[R, E, B]): ZQuery[R, E, (A, B)] = fa.zipPar(fb)
    def flatMap[A, B](fa: ZQuery[R, E, A], f: A => ZQuery[R, E, B]): ZQuery[R, E, B] = fa.flatMap(f)
  }
}
