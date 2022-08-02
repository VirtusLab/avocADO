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

  test("correctly expand a simple option comprehension 4") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
        c <- Some(a + b)
        d <- Some(4)
      } yield c + d
    }
    assertEquals(res, Some(7))
  }

  test("correctly expand a simple option comprehension 5") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
        c <- Some(a + b)
        d <- Some(4)
        e <- Some(d + 1)
      } yield c + d + e
    }
    assertEquals(res, Some(12))
  }

  test("correctly expand a simple option comprehension 6") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
        c <- Some(a + b)
        d <- None.asInstanceOf[Option[Int]]
      } yield c + d
    }
    assertEquals(res, None)
  }

  test("correctly expand a simple option comprehension 7") {
    val res = ado {
      for {
        a <- Some(1)
        aa <- None
        b <- Some(2)
        c <- Some(a + b)
      } yield c + 1
    }
    assertEquals(res, None)
  }

  test("correctly expand a simple option comprehension 8") {
    val res = ado {
      for {
        a <- Some(1)
      } yield a
    }
    assertEquals(res, Some(1))
  }

}