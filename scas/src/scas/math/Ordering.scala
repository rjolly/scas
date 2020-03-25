package scas.math

trait Ordering[T] extends PartialOrdering[T] with scala.math.Ordering[T] {
  def (x: T) < (y: T) = lt(x, y)
  def (x: T) <=(y: T) = lteq(x, y)
  def (x: T) > (y: T) = gt(x, y)
  def (x: T) >=(y: T) = gteq(x, y)
}
