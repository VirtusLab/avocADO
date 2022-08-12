package avacado.tests

import avocado.*
import scala.concurrent.duration._
import zio.*

class ZIOTests extends munit.FunSuite {

  given Applicative[UIO] = new Applicative[UIO] {
    def pure[A](a: A): UIO[A] = ZIO.succeed(a)
    def zip[A, B](fa: UIO[A], fb: UIO[B]): UIO[(A, B)] = fa.zipPar(fb)
  }

  def testWithTimeLimit(name: String, maxMillis: Long)(body: => Unit): Unit = {
    test(name) {
      val start = java.lang.System.currentTimeMillis()
      body
      val end = java.lang.System.currentTimeMillis()
      assert(end - start < maxMillis)
    }
  }

  testWithTimeLimit("correctly expand a simple ZIO comprehension 1", 1000) {
    val wait = ZIO.sleep(800.millis)
    val run: UIO[Int] = ado {
      for {
        a <- ZIO.succeed(1)
      } yield a
    }
    val res = Unsafe.unsafe { unsafe ?=>
      zio.Runtime.default.unsafe.run(
        run
      ).getOrThrowFiberFailure()
    }
    assertEquals(res, 1)
  }

  testWithTimeLimit("correctly expand a simple ZIO comprehension 2", 1000) {
    val wait = ZIO.sleep(800.millis)
    val run: UIO[Int] = ado {
      for {
        a <- wait.map(_ => 1)
        b <- wait.map(_ => 2)
      } yield a + b
    }
    val res = Unsafe.unsafe { unsafe ?=>
      zio.Runtime.default.unsafe.run(
        run
      ).getOrThrowFiberFailure()
    }
    assertEquals(res, 3)
  }

}
