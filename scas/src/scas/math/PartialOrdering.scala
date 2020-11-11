package scas.math

trait PartialOrdering[T] extends scala.math.PartialOrdering[T] with Equiv[T] {
  extension (x: T) {
    def <=(y: T): Boolean
    def >=(y: T) = y <= x
    def < (y: T) = x <= y && x <> y
    def > (y: T) = x >= y && x <> y
  }
}
