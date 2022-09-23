package avocado.tests

import zio.*

abstract class BaseZIO2Test extends munit.FunSuite {

  def testWithTimeLimit[R, E, A](name: String, maxMillis: Long)(body: ZIO[R, E, A])(expected: Either[E, A]): Unit = {
    test(name) {
      val start = java.lang.System.currentTimeMillis()
      val res: Either[E, A] = Unsafe.unsafe { unsafe ?=>
        zio.Runtime.default.unsafe.run(
          body.either.asInstanceOf[ZIO[Any, Any, Either[E, A]]]
        ).getOrThrowFiberFailure()
      }
      val end = java.lang.System.currentTimeMillis()
      assertEquals(res, expected)
      assert(end - start < maxMillis)
    }
  }

}
