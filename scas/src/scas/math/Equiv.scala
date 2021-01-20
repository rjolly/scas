package scas.math

trait Equiv[T] extends scala.math.Equiv[T] {
  extension[U] (x: U)(using c: U => T) {
    def ><(y: T): Boolean = c(x) >< y
    def <>(y: T): Boolean = c(x) <> y
  }
  extension (x: T) {
    def ><(y: T) = equiv(x, y)
    def <>(y: T) = !equiv(x, y)
  }
}
