package avocado.tests

import avocado.*

class ExplicitPipelineTests extends munit.FunSuite {

  given AvocADO[Option] = new AvocADO[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def map[A, B](fa: Option[A], f: A => B): Option[B] = fa.map(f)
    def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = fa.zip(fb)
    def flatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  test("handwritten pipeline 1") {
    val res = parallelize {
      Some(1).flatMap { a =>
        Some(2).map { b =>
          a + b
        }
      }
    }
    assertEquals(res, Some(3))
  }

  test("handwritten pipeline 2") {
    val res = parallelize {
      Some(1).flatMap { a =>
        Some(2).flatMap { b =>
          Some(a + b)
        }
      }
    }
    assertEquals(res, Some(3))
  }

}