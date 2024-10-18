package scas.util

import scala.concurrent.{ExecutionContext, CanAwait, Future}
import scala.concurrent.duration.Duration
import scala.util.{Try, Success}

class Lazy[+T](body: => T) extends Future[T] {
  lazy val await = body
  def value = Some(Success(await))
  def isCompleted = true
  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext) = ()
  def ready(atMost: Duration)(implicit permit: CanAwait) = this
  def result(atMost: Duration)(implicit permit: CanAwait) = await
  override def map[S](f: T => S)(implicit executor: ExecutionContext) = Lazy(f(await))
  override def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext) = f(await)
  def transform[S](f: Try[T] => Try[S])(implicit executor: ExecutionContext): Future[S] = ???
  def transformWith[S](f: Try[T] => Future[S])(implicit executor: ExecutionContext): Future[S] = ???
}
