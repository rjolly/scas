package scas.structure.commutative

import scas.base.Rational, Rational.given

trait Field[T] extends scas.structure.Field[T] with EuclidianDomain[T] {
  override def gcd(x: T, y: T) = if y.isZero then x else y
  override def pow(a: T, b: Rational) = if b.signum < 0 then pow(inverse(a), -b) else super.pow(a, b)
  extension (x: T) {
    override def divide(y: T) = super[Field].divide(x)(y)
    override def remainder(y: T) = zero
    override def divideAndRemainder(y: T) = (x / y, x % y)
  }
}

object Field {
  def apply[T : Field] = summon[Field[T]]

  trait Conv[T] extends Field[T] with scas.structure.Field.Conv[T] with UniqueFactorizationDomain.Conv[T]
}
