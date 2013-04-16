package scas.concurrent

import scala.concurrent.{ExecutionContext, CanAwait, Future}
import scala.concurrent.duration.Duration
import scala.util.{Try, Success}

trait Lazy[+T] extends Future[T] with (() => T) {
  def value = Some(Success(apply))
  def isCompleted = true
  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext) = ()
  def ready(atMost: Duration)(implicit permit: CanAwait) = this
  def result(atMost: Duration)(implicit permit: CanAwait) = apply
  override def map[S](f: T => S)(implicit executor: ExecutionContext) = Lazy(f(apply))
  override def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext) = f(apply)
}

object Lazy {
  def apply[T](body: => T) = new Lazy[T] {
    lazy val apply = body
  }
}
