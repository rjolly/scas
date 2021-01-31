package scas.structure.commutative

trait Field[T] extends EuclidianDomain[T] with scas.structure.Field[T] {
  override def gcd(x: T, y: T) = if (y.isZero) x else y
  extension (x: T) {
    override def divide(y: T) = super[Field]./(x)(y)
    override def remainder(y: T) = zero
    override def divideAndRemainder(y: T) = (x / y, x % y)
  }
}
