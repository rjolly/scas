package scas.structure.commutative

trait Field[T] extends EuclidianDomain[T] with scas.structure.Field[T] {
  override def gcd(x: T, y: T) = if (y.isZero) x else y
  extension (x: T) {
    override def / (y: T) = super[Field].extension_/(x)(y)
    override def % (y: T) = zero
    override def /%(y: T) = (x / y, x % y)
  }
}
