package avocado.tests

import avocado.*
import avocado.catseffect3.given

import scala.concurrent.duration.*
import cats.Monad
import cats.effect.kernel.Async
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

class CatsEffectTCTests extends munit.FunSuite {

  def testWithTimeLimit(name: String, maxMillis: Long)(body: => Unit): Unit = {
    test(name) {
      val start = System.currentTimeMillis()
      body
      val end = System.currentTimeMillis()
      assert(end - start < maxMillis)
    }
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 1", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- Monad[F].pure(1)
      } yield a
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 1)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 2", 1200) {
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

  testWithTimeLimit("cats effect comprehension with typeclasses 3", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
      } yield c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 4", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 5", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
        e <- wait
      } yield c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 6", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
        e <- wait
        f <- wait
        g <- wait.map(Function.const(7))
      } yield c + d + g
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 14)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 7", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
      } yield a + b
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 8", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 9", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        _ <- wait
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 10", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        _ <- wait.map(_ => a)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => 3)
      } yield a + b
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 11", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => {
          val a = 2
          a
        })
      } yield a + b
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 12", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => {
          val a = 2
          a
        })
        c <- wait.map { a =>
          def b(): Int = 3
          b()
        }
      } yield a + b + c
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 6)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 13", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
      } yield a + b + c
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 6)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 14", 1200) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
        d = 4
      } yield a + b + c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 10)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 15", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => b + 1)
        d = 4
      } yield a + b + c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 10)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 16", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => a + 2)
        d = 4
      } yield a + b + c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 10)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 17", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c = a + b
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 18", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d = 4
        e <- wait
      } yield c + d
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("cats effect comprehension with typeclasses 19", 2000) {
    val wait = IO.sleep(800.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
        e <- wait
        f <- wait
        g <- wait.map(Function.const(7))
      } yield c + d + g
    }
    val res = run[IO](wait).unsafeRunSync()
    assertEquals(res, 14)
  }

}
