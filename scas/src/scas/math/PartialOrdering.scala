package scas.math

trait PartialOrdering[T] extends scala.math.PartialOrdering[T] with Equiv[T] {
  extension (x: T) {
    inline def <=(y: T) = lteq(x, y)
    inline def >=(y: T) = gteq(x, y)
    def < (y: T) = lt(x, y)
    def > (y: T) = gt(x, y)
  }
}
