package avocado.tests

import avocado.*
import avocado.instances.catseffect2.given

import scala.concurrent.duration.*
import cats.effect.IO
import cats.effect.IO.given

class CatsEffect2Tests extends BaseCatsEffect2Test {

  testWithTimeLimit("cats effect 2 comprehension 1", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- IO(1)
      } yield a
    }
  }(1)

  testWithTimeLimit("cats effect 2 comprehension 2", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 2 comprehension 3", 1400) {
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

  testWithTimeLimit("cats effect 2 comprehension 4", 1400) {
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

  testWithTimeLimit("cats effect 2 comprehension 5", 1400) {
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

  testWithTimeLimit("cats effect 2 comprehension 6", 1400) {
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

  testWithTimeLimit("cats effect 2 comprehension 7", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
      } yield a + b
    }
  }(3)

  testWithTimeLimit("cats effect 2 comprehension 8", 900) {
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

  testWithTimeLimit("cats effect 2 comprehension 9", 900) {
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

  testWithTimeLimit("cats effect 2 comprehension 10", 1400) {
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

  testWithTimeLimit("cats effect 2 comprehension 11", 900) {
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

  testWithTimeLimit("cats effect 2 comprehension 12", 900) {
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

  testWithTimeLimit("cats effect 2 comprehension 13", 900) {
    val wait = IO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b = 2
        c <- wait.map(_ => 3)
      } yield a + b + c
    }
  }(6)

  testWithTimeLimit("cats effect 2 comprehension 14", 900) {
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

}
