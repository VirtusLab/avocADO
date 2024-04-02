package avocado.tests

import avocado.*
import avocado.instances.future.given

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FutureTests extends munit.FunSuite {

  def testWithTimeLimit[A](name: String, maxMillis: Long)(body: Future[A])(expected: A): Unit = {
    test(name) {
      val start = System.currentTimeMillis()
      val res = Await.result(body, maxMillis.millis)
      val end = System.currentTimeMillis()
      assertEquals(res, expected)
      assert(end - start < maxMillis)
    }
  }

  testWithTimeLimit("Future comprehension 1", 900) {
    def wait = Future(Thread.sleep(500))
    parallelize {
      for {
        a <- Future(1)
      } yield a
    }
  }(1)

  testWithTimeLimit("Future comprehension 2", 900) {
    def wait = Future(Thread.sleep(500))
    parallelize {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
  }(3)

  testWithTimeLimit("Future comprehension 3", 1400) {
    def wait = Future(Thread.sleep(500))
    parallelize {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
        c <- Future(a + b)
        d <- wait.map(_ => 4)
      } yield c + d
    }
  }(7)

}
