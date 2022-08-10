package avacado.tests

import avocado.*
import scala.concurrent.duration._
import cats.effect.IO
import cats.effect.unsafe.implicits._

class CatsEffectTests extends munit.FunSuite {

  given Applicative[IO] = new Applicative[IO] {
    def pure[A](a: A): IO[A] = IO(a)
    def zip[A, B](fa: IO[A], fb: IO[B]): IO[(A, B)] = fa.both(fb)
  }

  def testWithTimeLimit(name: String, maxMillis: Long)(body: => Unit): Unit = {
    test(name) {
      val start = System.currentTimeMillis()
      body
      val end = System.currentTimeMillis()
      assert(end - start < maxMillis)
    }
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 1", 1000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 2", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d <- wait.map(_ => 4)
      } yield c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 3", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 4", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d <- wait.map(_ => 4)
        e <- wait
      } yield c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 5", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d <- wait.map(_ => 4)
        e <- wait
        f <- wait
        g <- wait.map(Function.const(7))
      } yield c + d + g
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 14)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 6", 1000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
      } yield a + b
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 7", 1000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 8", 1000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        _ <- wait
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 9", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        _ <- wait.map(_ => a)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => 3)
      } yield a + b
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 10", 1000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => {
          val a = 2
          a
        })
      } yield a + b
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 3)
  }

  testWithTimeLimit("correctly expand a simple cats effect comprehension 11", 1000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
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
    val res = run.unsafeRunSync()
    assertEquals(res, 6)
  }

}
