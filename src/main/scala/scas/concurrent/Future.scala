package scas.concurrent

trait Future[+A] extends (() => A) {
  def map[B](f: A => B) = future(f(apply()))
  def flatMap[B](f: A => Future[B]) = f(apply())
}

object Future {
  def apply[A](value: => A) = new Future[A] {
    lazy val apply = value
  }
}
