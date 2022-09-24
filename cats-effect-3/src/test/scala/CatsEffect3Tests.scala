package avocado.tests

import avocado.*
import avocado.instances.catseffect3.given

import scala.concurrent.duration.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

class CatsEffect3Tests extends BaseCatsEffect3Test {

  testWithTimeLimit("cats effect 3 comprehension 1", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- IO(1)
      } yield a
    }
  }(1)

  testWithTimeLimit("cats effect 3 comprehension 2", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 3 comprehension 3", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d <- wait.map(_ => 4)
      } yield c + d
    }
  }(7)

  testWithTimeLimit("cats effect 3 comprehension 4", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
  }(7)

  testWithTimeLimit("cats effect 3 comprehension 5", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d <- wait.map(_ => 4)
        e <- wait
      } yield c + d
    }
  }(7)

  testWithTimeLimit("cats effect 3 comprehension 6", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
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
  }(14)

  testWithTimeLimit("cats effect 3 comprehension 7", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 3 comprehension 8", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 3 comprehension 9", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        _ <- wait
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 3 comprehension 10", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        _ <- wait.map(_ => a)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => 3)
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 3 comprehension 11", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => {
          val a = 2
          a
        })
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 3 comprehension 12", 900) {
    val wait = IO.sleep(500.millis)
    ado {
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
  }(6)

  testWithTimeLimit("cats effect 3 comprehension 13", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
      } yield a + b + c
    }
  }(6)

  testWithTimeLimit("cats effect 3 comprehension 14", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
        d = 4
      } yield a + b + c + d
    }
  }(10)

  testWithTimeLimit("cats effect 3 comprehension 15", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => b + 1)
        d = 4
      } yield a + b + c + d
    }
  }(10)

  testWithTimeLimit("cats effect 3 comprehension 16", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => a + 2)
        d = 4
      } yield a + b + c + d
    }
  }(10)

  testWithTimeLimit("cats effect 3 comprehension 17", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c = a + b
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
  }(7)

  testWithTimeLimit("cats effect 3 comprehension 18", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- IO(a + b)
        d = 4
        e <- wait
      } yield c + d
    }
  }(7)

  testWithTimeLimit("cats effect 3 comprehension 19", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
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
  }(14)

  testWithTimeLimit("cats effect 3 comprehension 20", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => a + 1)
        c <- wait.map(_ => 3)
        d <- wait.map(_ => a + 3)
      } yield a + b + c + d
    }
  }(10)

  testWithTimeLimit("cats effect 3 comprehension 21", 1400) {
    val wait = IO.sleep(500.millis)
    def getImplicit(using i: Int): IO[Int] = wait.map(_ => i)
    ado {
      for {
        a <- wait.map(_ => 1)
        given Int <- wait.map(_ => 2)
        c <- getImplicit
      } yield c
    }
  }(2)

  testWithTimeLimit("cats effect 3 comprehension 22", 1900) {
    val wait = IO.sleep(500.millis)
    def getImplicit(using i: Int): IO[Int] = wait.map(_ => i)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        given Int <- wait.map(_ => 3)
        c <- getImplicit
        given Int <- wait.map(_ => 5)
        d <- getImplicit
      } yield (c, d)
    }
  }((3, 5))

  testWithTimeLimit("cats effect 3 comprehension 23", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        _ <- wait.map(_ => 2)
        b = 1
      } yield b
    }
  }(1)

  testWithTimeLimit("cats effect 3 comprehension 24", 1400) {
    val wait = IO.sleep(500.millis)
    def getImplicit(using i: Int): IO[Int] = wait.map(_ => i)
    ado {
      for {
        _ <- wait.map(_ => 2)
        given Int = 1
        b <- getImplicit
      } yield b
    }
  }(1)

  testWithTimeLimit("cats effect 3 comprehension 25", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        (b: Int) = 2
        c <- wait.map(_ => b)
      } yield a + c
    }
  }(3)

  testWithTimeLimit("cats effect 3 comprehension 26", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        (b: Int) = a
        c <- wait.map(_ => b)
      } yield a + c
    }
  }(2)

  testWithTimeLimit("cats effect 3 comprehension 27", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        (e, f) = (1 -> 2)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 28", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        (g: Int) = 3
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 29", 1400) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        2 = 2
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 30", 1400) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        C(i) = C(5)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 31", 1400) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        cc@C(i) = C(5)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 32", 1400) {
    val wait = IO.sleep(500.millis)
    object obj {
      case class AClass[T](t: T)
    }
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        obj.AClass[Int](t) = obj.AClass(1)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 33", 1400) {
    val wait = IO.sleep(500.millis)
    object obj {
      case class AClass[T](t: T)
    }
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        obj.AClass[Int](1) = obj.AClass(1)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 34", 1400) {
    val wait = IO.sleep(500.millis)
    object obj {
      case class AClass[T](t: T)
    }
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        obj.AClass[Int](1) = obj.AClass(1): @unchecked
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 35", 1400) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        cc@C(i) = C(c)
        d <- wait.map(_ => 4)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 36", 1400) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        C(i) = C(a)
        d <- wait.map(_ => 4)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 37", 1900) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        _ <- wait
        C(i) = C(c)
      } yield c + b
    }
  }(5)

  testWithTimeLimit("cats effect 3 comprehension 38", 1900) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int*)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => a + 2)
        d <- wait.map(_ => 4)
        _ <- wait
        C(i, j, k) = C(1, 2, 3)
      } yield (c + b, i, j, k)
    }
  }((5, 1, 2, 3))

  testWithTimeLimit("cats effect 3 comprehension 39", 1900) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => 3)
        C(i) = C(1)
        d <- wait.map(_ => 4)
        _ <- wait
      } yield (c, i)
    }
  }((3, 1))

  testWithTimeLimit("cats effect 3 comprehension 40", 1900) {
    val wait = IO.sleep(500.millis)
    case class C(i: Int*)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => 3)
        C(i, j, k) = C(1, 2, 3)
        d <- wait.map(_ => 4)
        _ <- wait
      } yield (c + b, i, j, k)
    }
  }((5, 1, 2, 3))

}
