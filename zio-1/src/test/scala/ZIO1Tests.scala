package avocado.tests

import avocado.*
import avocado.instances.zio1.given
import scala.concurrent.duration.*
import zio.*
import zio.duration.*

class ZIO1Tests extends BaseZIO1Test {

  testWithTimeLimit("ZIO1 comprehension 1", 4000) {
    val wait = ZIO.sleep(1000.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
      } yield a
    }
  }(Right(1))

  testWithTimeLimit("ZIO1 comprehension 2", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
  }(Right(3))

  testWithTimeLimit("ZIO1 comprehension 3", 1400) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZIO.succeed(a + b)
        d <- wait.map(_ => 4)
      } yield c + d
    }
  }(Right(7))

  testWithTimeLimit("ZIO1 comprehension 4", 1400) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZIO.succeed(a + b)
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
  }(Right(7))

  testWithTimeLimit("ZIO1 comprehension 5", 1400) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZIO.succeed(a + b)
        d <- wait.map(_ => 4)
        e <- wait
      } yield c + d
    }
  }(Right(7))

  testWithTimeLimit("ZIO1 comprehension 6", 1400) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZIO.succeed(a + b)
        d <- wait.map(_ => 4)
        e <- wait
        f <- wait
        g <- wait.map(Function.const(7))
      } yield c + d + g
    }
  }(Right(14))

  testWithTimeLimit("ZIO1 comprehension 7", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
      } yield a + b
    }
  }(Right(3))
  
  testWithTimeLimit("ZIO1 comprehension 8", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
  }(Right(3))

  testWithTimeLimit("ZIO1 comprehension 9", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        _ <- wait
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
        c <- wait.map(_ => 3)
      } yield a + b
    }
  }(Right(3))

  testWithTimeLimit("ZIO1 comprehension 10", 1400) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        _ <- wait.map(_ => a)
        b <- wait.map(_ => 2)
        c <- wait.map(_ => 3)
      } yield a + b
    }
  }(Right(3))

  testWithTimeLimit("ZIO1 comprehension 11", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => {
          val a = 2
          a
        })
      } yield a + b
    }
  }(Right(3))

  testWithTimeLimit("ZIO1 comprehension 12", 900) {
    val wait = ZIO.sleep(500.millis)
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
  }(Right(6))

  testWithTimeLimit("ZIO1 comprehension 13", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZIO.fail("Sadge")
      } yield a + b
    }
  }(Left("Sadge"))

  testWithTimeLimit("ZIO1 comprehension 14", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- if a + b < 5 then ZIO.fail("Sadge") else ZIO.succeed(3)
      } yield a + b + c
    }
  }(Left("Sadge"))

}
