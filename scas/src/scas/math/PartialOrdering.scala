package scas.math

trait PartialOrdering[T] extends scala.math.PartialOrdering[T] with Equiv[T] {
  extension (x: T) {
    def <=(y: T) = lteq(x, y)
    def >=(y: T) = gteq(x, y)
    def < (y: T) = lt(x, y)
    def > (y: T) = gt(x, y)
  }
}
