package scas.math

trait Equiv[T] extends scala.math.Equiv[T] {
  extension (x: T) {
    def ><(y: T) = equiv(x, y)
    def <>(y: T) = !equiv(x, y)
  }
}
