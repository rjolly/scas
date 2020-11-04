package scas.math

trait Equiv[T] {
  extension (x: T) {
    def ><(y: T) = equiv(x, y)
    def <>(y: T) = !equiv(x, y)
  }
  def equiv(x: T, y: T): Boolean
}
