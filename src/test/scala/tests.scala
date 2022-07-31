package avacado.examples

import avocado.*

class tests extends munit.FunSuite {

  test("correctly expand a simple option comprehension 1") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
      } yield a + b
    }
    assertEquals(res, Some(3))
  }

  test("correctly expand a simple option comprehension 2") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
        c <- Some(3)
      } yield a + b + c
    }
    assertEquals(res, Some(6))
  }

  test("correctly expand a simple option comprehension 3") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
        c <- Some(3)
      } yield a + b
    }
    assertEquals(res, Some(3))
  }

}