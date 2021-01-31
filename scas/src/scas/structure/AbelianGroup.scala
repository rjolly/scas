package scas.structure

trait AbelianGroup[T] extends Structure[T] {
  extension[U] (x: U)(using c: U => T) {
    def + (y: T): T = c(x).add(y)
    def - (y: T): T = c(x).subtract(y)
  }
  extension (x: T) {
    def +[U](y: U)(using c: U => T): T = x.add(c(y))
    def -[U](y: U)(using c: U => T): T = x.subtract(c(y))
    def add(y: T): T
    def subtract(y: T): T
    def unary_- = zero - x
  }
  def abs(x: T) = if (x.signum < 0) -x else x
  extension (x: T) def signum: Int
  def zero: T
  extension (x: T) def isZero = x >< zero
}
