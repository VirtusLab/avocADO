package avocado.tests

import avocado.*

class OptionTests extends munit.FunSuite {

  given AvocADO[Option] = new AvocADO[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def map[A, B](fa: Option[A], f: A => B): Option[B] = fa.map(f)
    def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = fa.zip(fb)
    def flatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  test("option comprehension 1") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
      } yield a + b
    }
    assertEquals(res, Some(3))
  }

  test("option comprehension 2") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
        c <- Some(3)
      } yield a + b + c
    }
    assertEquals(res, Some(6))
  }

  test("option comprehension 3") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(2)
        c <- Some(3)
      } yield a + b
    }
    assertEquals(res, Some(3))
  }

  test("option comprehension 4") {
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

  test("option comprehension 5") {
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

  test("option comprehension 6") {
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

  test("option comprehension 7") {
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

  test("option comprehension 8") {
    val res = ado {
      for {
        a <- Some(1)
      } yield a
    }
    assertEquals(res, Some(1))
  }

  test("option comprehension 9") {
    val res = ado {
      for {
        a <- Some(1)
        a <- Some(a)
        a <- Some(a)
      } yield a
    }
    assertEquals(res, Some(1))
  }

  test("option comprehension 10") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(a + 2)
        a <- Some(3)
      } yield a + b
    }
    assertEquals(res, Some(6))
  }

  test("option comprehension 11") {
    val res = ado {
      for {
        a <- Some(1)
        b <- Some(a + 2)
        a <- if b < 1 then None else Some("1")
      } yield a.toInt + b
    }
    assertEquals(res, Some(4))
  }

  test("option comprehension 12") {
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

  test("option comprehension 13") {
    val res = ado {
      for {
        a <- Some(1)
        a <- Some(2)
      } yield a
    }
    assertEquals(res, Some(2))
  }

  test("option comprehension 14") {
    def getImplicit(using i: Int): Option[Int] = Some(i)
    val res = ado {
      for {
        given Int <- Some(2)
        b <- getImplicit
      } yield b
    }
    assertEquals(res, Some(2))
  }

  test("option comprehension 15") {
    def getImplicit(using i: Int): Option[Int] = Some(i)
    val res = ado {
      for {
        a <- Some(2)
        given Int = 1
        b <- getImplicit
      } yield b
    }
    assertEquals(res, Some(1))
  }

  test("option comprehension 16") {
    val res = ado {
      for {
        _ <- Some(2)
        b = 1
      } yield b
    }
    assertEquals(res, Some(1))
  }

  test("option comprehension 17") {
    def getImplicit(using i: Int): Option[Int] = Some(i)
    val res = ado {
      for {
        _ <- Some(2)
        given Int = 1
        b <- getImplicit
      } yield b
    }
    assertEquals(res, Some(1))
  }


  test("option comprehension 18") {
    val res = ado {
      for {
        a <- Some(1)
        (b: Int) = 2
        c <- Some(b)
      } yield a + c
    }
    assertEquals(res, Some(3))
  }

  test("option comprehension 19") {
    val res = ado {
      for {
        a <- Some(1)
        _ <- Some(2)
        (_, a) = (3, 4)
      } yield a
    }
    assertEquals(res, Some(4))
  }

  test("option comprehension 20") {
    val res = ado {
      for {
        a <- Some(1)
        _ <- Some(2)
        (_, (_, a)) = (3, (4, 5))
      } yield a
    }
    assertEquals(res, Some(5))
  }

  test("option comprehension 21") {
    case class C(i: Int, j: Int)
    val res = ado {
      for {
        a <- Some(1)
        _ <- Some(2)
        C(_, a) = C(3, 4)
      } yield a
    }
    assertEquals(res, Some(4))
  }

  test("option comprehension 22") {
    case class C(i: Int*)
    val res = ado {
      for {
        a <- Some(1)
        _ <- Some(2)
        C(_, a) = C(3, 4)
      } yield a
    }
    assertEquals(res, Some(4))
  }

}
