package avocado.catseffect3

import avocado.AvocADO
import _root_.cats.effect.IO

given AvocADO[IO] = new AvocADO[IO] {
  def pure[A](a: A): IO[A] = IO(a)
  def map[A, B](fa: IO[A], f: A => B): IO[B] = fa.map(f)
  def zip[A, B](fa: IO[A], fb: IO[B]): IO[(A, B)] = fa.both(fb)
  def flatMap[A, B](fa: IO[A], f: A => IO[B]): IO[B] = fa.flatMap(f)
}
