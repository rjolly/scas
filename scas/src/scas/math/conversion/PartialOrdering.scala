package scas.math.conversion

trait PartialOrdering[T] extends scas.math.PartialOrdering[T] with Equiv[T] {
  extension[U] (x: U)(using c: U => T) {
    def <=(y: T) = lteq(c(x), y)
    def >=(y: T) = gteq(c(x), y)
    def < (y: T) = lt(c(x), y)
    def > (y: T) = gt(c(x), y)
  }
  extension (x: T) {
    def <=[U](y: U)(using c: U => T) = lteq(x, c(y))
    def >=[U](y: U)(using c: U => T) = gteq(x, c(y))
    def < [U](y: U)(using c: U => T) = lt(x, c(y))
    def > [U](y: U)(using c: U => T) = gt(x, c(y))
  }
}
