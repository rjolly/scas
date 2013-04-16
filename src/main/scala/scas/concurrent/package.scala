package scas

import scala.concurrent.{ExecutionContext, Future}

package object concurrent {
  val enable = System.getProperty("future.enable", false.toString) match {
    case "" => true
    case s => s.toBoolean
  }

  def future[T](body: => T)(implicit executor: ExecutionContext) = if (enable) Future(body) else Lazy(body)
}
