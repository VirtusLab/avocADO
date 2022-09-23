package avocado.tests

import cats.effect.IO
import cats.effect.Timer
import cats.effect.ContextShift
import scala.concurrent.ExecutionContext

abstract class BaseCatsEffect2Test extends munit.FunSuite {

  def testWithTimeLimit[A](name: String, maxMillis: Long)(body: IO[A])(expected: A): Unit = {
    test(name) {
      val start = System.currentTimeMillis()
      val res = body.unsafeRunSync()
      val end = System.currentTimeMillis()
      assertEquals(res, expected)
      assert(end - start < maxMillis)
    }
  }

  given Timer[IO] = IO.timer(ExecutionContext.global)
  given ContextShift[IO] = IO.contextShift(ExecutionContext.global)

}
