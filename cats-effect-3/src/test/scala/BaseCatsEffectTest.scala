package avocado.tests

import cats.effect.IO
import cats.effect.unsafe.implicits.*

abstract class BaseCatsEffectTest extends munit.FunSuite {

  def testWithTimeLimit[A](name: String, maxMillis: Long)(body: IO[A])(expected: A): Unit = {
    test(name) {
      val start = System.currentTimeMillis()
      val res = body.unsafeRunSync()
      val end = System.currentTimeMillis()
      assertEquals(res, expected)
      assert(end - start < maxMillis)
    }
  }

}
