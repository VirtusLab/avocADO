package avocado.tests

import avocado.*
// import avocado.catseffect3.given

import scala.concurrent.duration.*
import cats.Monad
import cats.effect.kernel.Async
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

class CatsEffectTCTests extends munit.FunSuite {

  given [F[_]: Monad: Async] : AvocADO[F] = new AvocADO[F] {
    def pure[A](a: A): F[A] = Monad[F].pure(a)
    def map[A, B](fa: F[A], f: A => B): F[B] = Monad[F].map(fa)(f)
    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = Async[F].both(fa, fb)
    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }

  def testWithTimeLimit(name: String, maxMillis: Long)(body: => Unit): Unit = {
    test(name) {
      val start = System.currentTimeMillis()
      body
      val end = System.currentTimeMillis()
      assert(end - start < maxMillis)
    }
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 1", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
      } yield a
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 1)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 2", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 3)
  }

}
