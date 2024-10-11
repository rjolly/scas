package scas.util

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

extension [U](x: U)
  def unary_~[T](using c: U => T) = c(x)

type Conversion[T] = [X] =>> X => T

type ToFrags[T] = [X] =>> Fragable[X, T]

given ExecutionContext = ExecutionContext.global

type Future[T] = scala.concurrent.Future[T]

val Future = scala.concurrent.Future

extension[T](s: Future[T])
  def await = Await.result(s, Duration.Inf)
