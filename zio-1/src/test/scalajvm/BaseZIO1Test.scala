package avocado.tests

import zio.*

abstract class BaseZIO1Test extends munit.FunSuite {

  def testWithTimeLimit[R, E, A](name: String, maxMillis: Long)(body: ZIO[R, E, A])(expected: Either[E, A]): Unit = {
    test(name) {
      val runtime = Runtime.default
      val start = java.lang.System.currentTimeMillis()
      val res: Either[E, A] = 
        runtime.unsafeRun(
          body.either.asInstanceOf[ZIO[Any, Any, Either[E, A]]]
        )
      val end = java.lang.System.currentTimeMillis()
      assertEquals(res, expected)
      assert(end - start < maxMillis)
    }
  }

}
