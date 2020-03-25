package scas.math

trait Numeric[T] extends Ordering[T] with scala.math.Numeric[T] {
  def (x: T) + (y: T) = plus(x, y)
  def (x: T) - (y: T) = minus(x, y)
}
