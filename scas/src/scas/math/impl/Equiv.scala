package scas.math.impl

trait Equiv[T] extends scala.math.Equiv[T] {
  extension (x: T) {
    inline def ><(y: T) = equiv(x, y)
    inline def <>(y: T) = !equiv(x, y)
  }
}
