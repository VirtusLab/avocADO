package avocado.tests

import avocado.*
import avocado.instances.zio2.given
import scala.concurrent.duration.*
import zio.*

class ZIOTests extends BaseZIOTest {

  testWithTimeLimit("ZIO comprehension 1", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- ZIO.succeed(1)
      } yield a
    }
  }(Right(1))

  testWithTimeLimit("ZIO comprehension 2", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
  }(Right(3))

  testWithTimeLimit("ZIO comprehension 3", 1400) {
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

  testWithTimeLimit("ZIO comprehension 4", 1400) {
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

  testWithTimeLimit("ZIO comprehension 5", 1400) {
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

  testWithTimeLimit("ZIO comprehension 6", 1400) {
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

  testWithTimeLimit("ZIO comprehension 7", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        _ <- wait
      } yield a + b
    }
  }(Right(3))
  
  testWithTimeLimit("ZIO comprehension 8", 900) {
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

  testWithTimeLimit("ZIO comprehension 9", 900) {
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

  testWithTimeLimit("ZIO comprehension 10", 1400) {
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

  testWithTimeLimit("ZIO comprehension 11", 900) {
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

  testWithTimeLimit("ZIO comprehension 12", 900) {
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

  testWithTimeLimit("ZIO comprehension 13", 900) {
    val wait = ZIO.sleep(500.millis)
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZIO.fail("Sadge")
      } yield a + b
    }
  }(Left("Sadge"))

  testWithTimeLimit("ZIO comprehension 14", 900) {
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
