package scas.math

trait PartialOrdering[T] extends Equiv[T] {
  def (x: T) <=(y: T): Boolean
  def (x: T) >=(y: T) = y <= x
  def (x: T) < (y: T) = x <= y && x <> y
  def (x: T) > (y: T) = x >= y && x <> y
  def equiv(x: T, y: T) = x <= y && y <= x
}
