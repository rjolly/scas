package scas.structure.commutative

trait Field[T] extends scas.structure.Field[T] with EuclidianDomain[T] {
  override def gcd(x: T, y: T) = if (y.isZero) x else y
  extension (x: T) {
    override def divide(y: T) = super[Field].divide(x)(y)
    override def remainder(y: T) = zero
    override def divideAndRemainder(y: T) = (x / y, x % y)
  }
}
