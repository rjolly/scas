package scas.util

import scala.concurrent.{ExecutionContext, CanAwait, Future}
import scala.concurrent.duration.Duration
import scala.util.{Try, Success}

class Lazy[+T](body: => T) extends Future[T] {
  lazy val await = body
  def value = Some(Success(await))
  def isCompleted = true
  def onComplete[U](func: Try[T] => U)(using executor: ExecutionContext) = ()
  def ready(atMost: Duration)(using permit: CanAwait) = this
  def result(atMost: Duration)(using permit: CanAwait) = await
  override def map[S](f: T => S)(using executor: ExecutionContext) = Lazy(f(await))
  override def flatMap[S](f: T => Future[S])(using executor: ExecutionContext) = f(await)
  def transform[S](f: Try[T] => Try[S])(using executor: ExecutionContext): Future[S] = ???
  def transformWith[S](f: Try[T] => Future[S])(using executor: ExecutionContext): Future[S] = ???
}
