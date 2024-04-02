package avocado.tests

import zio.*
import zio.query.*

abstract class BaseZQueryTest extends munit.FunSuite {

  def testWithTimeLimit[R, E, A](name: String, maxMillis: Long)(body: ZQuery[R, E, A])(expected: Either[E, A]): Unit = {
    test(name) {
      val start = java.lang.System.currentTimeMillis()
      val res: Either[E, A] = Unsafe.unsafe { unsafe ?=>
        zio.Runtime.default.unsafe.run(
          body.either.run.asInstanceOf[ZIO[Any, Any, Either[E, A]]]
        ).getOrThrowFiberFailure()
      }
      val end = java.lang.System.currentTimeMillis()
      assertEquals(res, expected)
      assert(end - start < maxMillis)
    }
  }
}
