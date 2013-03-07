package scas

package object concurrent {
  def future[A](value: => A) = Future(value)
  val ExecutionContext = scala.concurrent.ExecutionContext
}
