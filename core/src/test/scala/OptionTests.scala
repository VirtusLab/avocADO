package avacado.tests

import avocado.*

class OptionTests extends munit.FunSuite {

  given Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = fa.zip(fb)
  }

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

  test("correctly expand a simple option comprehension 9") {
    val res = ado {
      for {
        a <- Some(1)
        a <- Some(a)
        a <- Some(a)
      } yield a
    }
    assertEquals(res, Some(1))
  }

  test("correctly expand a simple option comprehension 10") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(a + 2)
        a <- Some(3)
      } yield a + b
    }
    assertEquals(res, Some(6))
  }

  test("correctly expand a simple option comprehension 11") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(a + 2)
        a <- if b < 1 then None else Some("1")
      } yield a.toInt + b
    }
    assertEquals(res, Some(4))
  }

  test("correctly expand a simple option comprehension 12") {
    val res = ado {
      for {
        _ <- Option(null)
        xd <- Option {
          1 + "2".toInt
        }
      } yield xd
    }
    assertEquals(res, None)
  }

}
