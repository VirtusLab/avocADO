package avacado.examples

import avocado.*

class tests extends munit.FunSuite {

  test("correctly expand a simple option comprehension") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
      } yield a + b
    }
    assertEquals(res, Some(3))
  }

}