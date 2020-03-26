package scas.math

trait Equiv[T] {
  def (x: T) ><(y: T) = equiv(x, y)
  def (x: T) <>(y: T) = !equiv(x, y)
  def equiv(x: T, y: T): Boolean
}
