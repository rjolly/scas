package scas.structure

trait AbelianGroup[T] extends Structure[T] {
  extension (x: T) {
    def add(y: T): T
    def subtract(y: T): T
    inline def + (y: T) = x.add(y)
    inline def - (y: T) = x.subtract(y)
    def unary_- = zero - x
    def isZero = x >< zero
    def signum: Int
  }
  def abs(x: T) = if (x.signum < 0) -x else x
  def zero: T
}
