package scas.structure.commutative.impl

trait Field[T] extends scas.structure.impl.Field[T] with EuclidianDomain[T] {
  override def gcd(x: T, y: T) = if (y.isZero) x else y
  extension (x: T) {
    override def divide(y: T) = super[Field].divide(x)(y)
    override def remainder(y: T) = zero
    override def divideAndRemainder(y: T) = (x / y, x % y)
  }
}

object Field {
  def apply[T : Field] = summon[Field[T]]
}