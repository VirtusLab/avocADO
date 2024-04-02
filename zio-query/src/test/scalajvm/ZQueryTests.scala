package avocado.tests

import avocado.*
import avocado.instances.zioquery.given
import scala.concurrent.duration.*
import zio.*
import zio.query.ZQuery

class ZQueryTests extends BaseZQueryTest {

  testWithTimeLimit("ZQuery comprehension 1", 4000) {
    val wait = ZQuery.fromZIO(ZIO.sleep(1000.millis))
    ado {
      for {
        a <- wait.map(_ => 1)
      } yield a
    }
  }(Right(1))

  testWithTimeLimit("ZQuery comprehension 2", 900) {
    val wait = ZQuery.fromZIO(ZIO.sleep(500.millis))
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
  }(Right(3))

  testWithTimeLimit("ZQuery comprehension 3", 1400) {
    val wait = ZQuery.fromZIO(ZIO.sleep(500.millis))
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZQuery.fromZIO(ZIO.succeed(a + b))
        d <- wait.map(_ => 4)
      } yield c + d
    }
  }(Right(7))

  testWithTimeLimit("ZQuery comprehension 4", 1400) {
    val wait = ZQuery.fromZIO(ZIO.sleep(500.millis))
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZQuery.fromZIO(ZIO.succeed(a + b))
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
  }(Right(7))

  testWithTimeLimit("ZQuery comprehension 5", 1400) {
    val wait = ZQuery.fromZIO(ZIO.sleep(500.millis))
    ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- ZQuery.fromZIO(ZIO.succeed(a + b))
        d <- wait.map(_ => 4)
        e <- wait.map(Function.const(5))
      } yield c + d
    }
  }(Right(7))

}
