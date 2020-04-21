package scas.structure.commutative

trait Field[T] extends EuclidianDomain[T] with scas.structure.Field[T] {
  override def gcd(x: T, y: T) = if (y.isZero) x else y
  override def (x: T) / (y: T) = super[Field]./(x)(y)
  override def (x: T) % (y: T) = zero
  override def (x: T) /%(y: T) = (x / y, x % y)
}
