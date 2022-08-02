package avacado.tests

import avocado.*
import scala.concurrent.duration._
import cats.effect.IO
import cats.effect.unsafe.implicits._

class CatsEffectTests extends munit.FunSuite {

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

}
