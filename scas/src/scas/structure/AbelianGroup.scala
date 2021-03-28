package scas.structure

trait AbelianGroup[T] extends Structure[T] {
  extension[U] (x: U)(using c: U => T) {
    def + (y: T) = c(x).add(y)
    def - (y: T) = c(x).subtract(y)
    def unary_- : T = -c(x)
  }
  extension (x: T) {
    def +[U](y: U)(using c: U => T) = x.add(c(y))
    def -[U](y: U)(using c: U => T) = x.subtract(c(y))
    def add(y: T): T
    def subtract(y: T): T
    def unary_- = zero - x
    def isZero = x >< zero
    def signum: Int
  }
  def abs[U](x: U)(using c: U => T): T = abs(c(x))
  def abs(x: T) = if (x.signum < 0) -x else x
  def zero: T
}
