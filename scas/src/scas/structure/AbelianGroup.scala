package scas.structure

trait AbelianGroup[T] extends Structure[T] {
  extension (x: T) {
    def + (y: T): T
    def - (y: T): T
    def unary_- = zero - x
  }
  def abs(x: T) = if (x.signum < 0) -x else x
  extension (x: T) def signum: Int
  def zero: T
  extension (x: T) def isZero = x >< zero
}
