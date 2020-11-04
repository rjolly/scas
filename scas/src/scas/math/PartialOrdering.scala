package scas.math

trait PartialOrdering[T] extends Equiv[T] {
  extension (x: T) {
    def <=(y: T): Boolean
    def >=(y: T) = y <= x
    def < (y: T) = x <= y && x <> y
    def > (y: T) = x >= y && x <> y
  }
  def equiv(x: T, y: T) = x <= y && y <= x
}
