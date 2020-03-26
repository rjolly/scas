package scas.power

trait Ordering[T] extends scas.math.Ordering[T] {
  def compare(x: T, y: T) = compares(x, y)
  def compares(x: T, y: T): Int
}
