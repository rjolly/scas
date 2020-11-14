package scas.math

trait Equiv[T] extends scala.math.Equiv[T] {
  extension[U] (x: U)(using Conversion[U, T]) {
    def ><(y: T): Boolean = (x: T) >< y
    def <>(y: T): Boolean = (x: T) <> y
  }
  extension (x: T) {
    def ><(y: T) = equiv(x, y)
    def <>(y: T) = !equiv(x, y)
  }
}
