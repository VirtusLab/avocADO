package avocado.instances

import avocado.AvocADO
import scala.concurrent.{ Future, ExecutionContext }

object future {
  given (using ExecutionContext): AvocADO[Future] = new AvocADO[Future] {
    def pure[A](a: A): Future[A] = Future.successful(a)
    def map[A, B](fa: Future[A], f: A => B): Future[B] = fa.map(f)
    def zip[A, B](fa: Future[A], fb: Future[B]): Future[(A, B)] = fa.zip(fb)
    def flatMap[A, B](fa: Future[A], f: A => Future[B]): Future[B] = fa.flatMap(f)
  }
}
