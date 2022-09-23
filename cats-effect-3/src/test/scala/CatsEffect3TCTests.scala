package avocado.tests

import avocado.*
import avocado.instances.catseffect3.given

import scala.concurrent.duration.*
import cats.Monad
import cats.effect.kernel.Async
import cats.syntax.all.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

class CatsEffectTCTests extends BaseCatsEffect3Test {

  testWithTimeLimit("cats effect comprehension with typeclasses 1", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- Monad[F].pure(1)
      } yield a
    }
    run[IO](wait)
  }(1)

  testWithTimeLimit("cats effect comprehension with typeclasses 2", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
    run[IO](wait)
  }(3)

  testWithTimeLimit("cats effect comprehension with typeclasses 3", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
      } yield c + d
    }
    run[IO](wait)
  }(7)

  testWithTimeLimit("cats effect comprehension with typeclasses 4", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
    run[IO](wait)
  }(7)

  testWithTimeLimit("cats effect comprehension with typeclasses 5", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d <- wait.map(_ => 4)
        e <- wait
      } yield c + d
    }
    run[IO](wait)
  }(7)

  testWithTimeLimit("cats effect comprehension with typeclasses 6", 1400) {
    val wait = IO.sleep(500.millis)
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
    run[IO](wait)
  }(14)

  testWithTimeLimit("cats effect comprehension with typeclasses 7", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
      } yield a + b
    }
    run[IO](wait)
  }(3)

  testWithTimeLimit("cats effect comprehension with typeclasses 8", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
    run[IO](wait)
  }(3)

  testWithTimeLimit("cats effect comprehension with typeclasses 9", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        _ <- wait
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
    run[IO](wait)
  }(3)

  testWithTimeLimit("cats effect comprehension with typeclasses 10", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        _ <- wait.map(_ => a)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => 3)
      } yield a + b
    }
    run[IO](wait)
  }(3)

  testWithTimeLimit("cats effect comprehension with typeclasses 11", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => {
          val a = 2
          a
        })
      } yield a + b
    }
    run[IO](wait)
  }(3)

  testWithTimeLimit("cats effect comprehension with typeclasses 12", 900) {
    val wait = IO.sleep(500.millis)
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
    run[IO](wait)
  }(6)

  testWithTimeLimit("cats effect comprehension with typeclasses 13", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
      } yield a + b + c
    }
    run[IO](wait)
  }(6)

  testWithTimeLimit("cats effect comprehension with typeclasses 14", 900) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
        d = 4
      } yield a + b + c + d
    }
    run[IO](wait)
  }(10)

  testWithTimeLimit("cats effect comprehension with typeclasses 15", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => b + 1)
        d = 4
      } yield a + b + c + d
    }
    run[IO](wait)
  }(10)

  testWithTimeLimit("cats effect comprehension with typeclasses 16", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => a + 2)
        d = 4
      } yield a + b + c + d
    }
    run[IO](wait)
  }(10)

  testWithTimeLimit("cats effect comprehension with typeclasses 17", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c = a + b
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
    run[IO](wait)
  }(7)

  testWithTimeLimit("cats effect comprehension with typeclasses 18", 1400) {
    val wait = IO.sleep(500.millis)
    def run[F[_]: Monad: Async](wait: F[Unit]): F[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Monad[F].pure(a + b)
        d = 4
        e <- wait
      } yield c + d
    }
    run[IO](wait)
  }(7)

  testWithTimeLimit("cats effect comprehension with typeclasses 19", 1400) {
    val wait = IO.sleep(500.millis)
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
    run[IO](wait)
  }(14)

}
