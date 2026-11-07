package scas.math

trait Integral[T] extends Numeric[T] {
  extension (x: T) {
    def / (y: T): T
    def % (y: T): T
    def /%(y: T) = (x / y, x % y)
  }
}
