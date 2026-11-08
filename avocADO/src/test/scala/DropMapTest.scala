package avocado.tests

import avocado.*

class DropMapTest extends munit.FunSuite {
  class myOptionPackage(doOnMap: => Unit) {
    sealed trait MyOption[+A] {
      def map[B](f: A => B): MyOption[B] = this match {
        case MySome(a) =>
          doOnMap
          MySome(f(a))
        case MyNone => MyNone
      }
      def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
        case MySome(a) => f(a)
        case MyNone => MyNone
      }
      def zip[B](that: MyOption[B]): MyOption[(A, B)] = (this, that) match {
        case (MySome(a), MySome(b)) => MySome((a, b))
        case _ => MyNone
      }
      def value: Option[A] = this match {
        case MySome(a) => Some(a)
        case MyNone => None
      }
    }
    case class MySome[A](a: A) extends MyOption[A]
    case object MyNone extends MyOption[Nothing]
  }

  test("don't drop map in a simple case") {
    val (resOrg, mapUnusedResOrg) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      (for {
        a <- MySome(1)
        b <- MySome(2)
      } yield a + b
      ).value -> mapUsed
    }
    val (res, mapUnusedRes) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      dropUnusedMap(
        for {
          a <- MySome(1)
          b <- MySome(2)
        } yield a + b
      ).value -> mapUsed
    }
    assertEquals(res, resOrg)
    assert(mapUnusedRes == mapUnusedResOrg)
  }

  test("drop map with same var ref as result") {
    val (resOrg, mapUnusedResOrg) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      (for {
        a <- MySome(1)
        b <- MySome(a)
      } yield b
      ).value -> mapUsed
    }
    val (res, mapUnusedRes) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      dropUnusedMap(
        for {
          a <- MySome(1)
          b <- MySome(a)
        } yield b
      ).value -> mapUsed
    }
    assertEquals(res, resOrg)
    assert(mapUnusedRes < mapUnusedResOrg)
  }

  test("drop map with unit result and wildcard last pattern") {
    val (resOrg, mapUnusedResOrg) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      (for {
        a <- MySome(1)
        _ <- MySome(())
      } yield ()
      ).value -> mapUsed
    }
    val (res, mapUnusedRes) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      dropUnusedMap(
        for {
          a <- MySome(1)
          _ <- MySome(())
        } yield ()
      ).value -> mapUsed
    }
    assertEquals(res, resOrg)
    assert(mapUnusedRes < mapUnusedResOrg)
  }

  test("drop map with unit result and named last pattern") {
    val (resOrg, mapUnusedResOrg) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      (for {
        a <- MySome(1)
        b <- MySome(())
      } yield ()
      ).value -> mapUsed
    }
    val (res, mapUnusedRes) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      dropUnusedMap(
        for {
          a <- MySome(1)
          b <- MySome(())
        } yield ()
      ).value -> mapUsed
    }
    assertEquals(res, resOrg)
    assert(mapUnusedRes < mapUnusedResOrg)
  }

  test("drop map with unit result and wildcard last pattern with alias in the middle") {
    val (resOrg, mapUnusedResOrg) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      (for {
        a <- MySome(1)
        b = a
        _ <- MySome(())
      } yield ()
      ).value -> mapUsed
    }
    val (res, mapUnusedRes) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      dropUnusedMap(
        for {
          a <- MySome(1)
          b = a
          _ <- MySome(())
        } yield ()
      ).value -> mapUsed
    }
    assertEquals(res, resOrg)
    assert(mapUnusedRes < mapUnusedResOrg)
  }

  test("drop map with unit result and named last pattern with alias in the middle") {
    val (resOrg, mapUnusedResOrg) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      (for {
        a <- MySome(1)
        b = a
        c <- MySome(())
      } yield ()
      ).value -> mapUsed
    }
    val (res, mapUnusedRes) = {
      var mapUsed = 0
      val myOption = new myOptionPackage({ mapUsed = mapUsed + 1 })
      import myOption.*
      dropUnusedMap(
        for {
          a <- MySome(1)
          b = a
          c <- MySome(())
        } yield ()
      ).value -> mapUsed
    }
    assertEquals(res, resOrg)
    assert(mapUnusedRes < mapUnusedResOrg)
  }
}
