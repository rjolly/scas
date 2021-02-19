package scas.math

trait Equiv[T] extends scala.math.Equiv[T] {
  extension[U] (x: U)(using c: U => T) {
    def ><(y: T) = equiv(c(x), y)
    def <>(y: T) = !equiv(c(x), y)
  }
  extension (x: T) {
    def ><[U](y: U)(using c: U => T) = equiv(x, c(y))
    def <>[U](y: U)(using c: U => T) = !equiv(x, c(y))
  }
}
