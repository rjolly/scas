package scas.concurrent

import scas.concurrent.duration.Duration

object Await {
  def result[A](future: Future[A], duration: Duration) = future()
}
