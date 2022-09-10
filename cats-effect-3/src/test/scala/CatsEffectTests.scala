package avacado.tests

import avocado.*
import avocado.catseffect3.given

import scala.concurrent.duration.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

class CatsEffectTests extends munit.FunSuite {

  def testWithTimeLimit(name: String, maxMillis: Long)(body: => Unit): Unit = {
    test(name) {
      val start = System.currentTimeMillis()
      body
      val end = System.currentTimeMillis()
      assert(end - start < maxMillis)
    }
  }

  testWithTimeLimit("cats effect comprehension 1", 1200) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- IO(1)
      } yield a
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 1)
  }

  testWithTimeLimit("cats effect comprehension 2", 1200) {
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

  testWithTimeLimit("cats effect comprehension 3", 2000) {
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

  testWithTimeLimit("cats effect comprehension 4", 2000) {
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

  testWithTimeLimit("cats effect comprehension 5", 2000) {
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

  testWithTimeLimit("cats effect comprehension 6", 2000) {
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

  testWithTimeLimit("cats effect comprehension 7", 1200) {
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

  testWithTimeLimit("cats effect comprehension 8", 1200) {
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

  testWithTimeLimit("cats effect comprehension 9", 1200) {
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

  testWithTimeLimit("cats effect comprehension 10", 2000) {
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

  testWithTimeLimit("cats effect comprehension 11", 1200) {
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

  testWithTimeLimit("cats effect comprehension 12", 1200) {
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

  testWithTimeLimit("cats effect comprehension 13", 1200) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
      } yield a + b + c
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 6)
  }

  testWithTimeLimit("cats effect comprehension 14", 1200) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
        d = 4
      } yield a + b + c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 10)
  }

  testWithTimeLimit("cats effect comprehension 15", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => b + 1)
        d = 4
      } yield a + b + c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 10)
  }

  testWithTimeLimit("cats effect comprehension 16", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => a + 2)
        d = 4
      } yield a + b + c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 10)
  }

  testWithTimeLimit("cats effect comprehension 17", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c = a + b
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("cats effect comprehension 18", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d = 4
        e <- wait
      } yield c + d
    }
    val res = run.unsafeRunSync()
    assertEquals(res, 7)
  }

  testWithTimeLimit("cats effect comprehension 19", 2000) {
    val wait = IO.sleep(800.millis)
    val run: IO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
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

}
