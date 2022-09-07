package avocado.catseffect3

import avocado.Applicative
import _root_.cats.effect.IO

given Applicative[IO] = new Applicative[IO] {
  def pure[A](a: A): IO[A] = IO(a)
  def zip[A, B](fa: IO[A], fb: IO[B]): IO[(A, B)] = fa.both(fb)
}
