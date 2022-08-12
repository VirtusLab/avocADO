package avacado.tests

import avocado.*

class ExplicitPipelineTests extends munit.FunSuite {

  given Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = fa.zip(fb)
  }

  test("correctly expand a simple handwritten pipeline 1") {
    val res = ado {
      Some(1).flatMap { a =>
        Some(2).map { b =>
          a + b
        }
      }
    }
    assertEquals(res, Some(3))
  }

  test("correctly expand a simple handwritten pipeline 2") {
    val res = ado {
      Some(1).flatMap { a =>
        Some(2).flatMap { b =>
          Some(a + b)
        }
      }
    }
    assertEquals(res, Some(3))
  }

}