package scas.structure.commutative

import scas.{BigInteger, int2bigInt}

trait Field[T] extends EuclidianDomain[T] with scas.structure.Field[T] with
  override def gcd(x: T, y: T) = if (norm(x) < norm(y)) y else x
  override def (x: T) / (y: T) = super[Field]./(x)(y)
  override def (x: T) % (y: T) = zero
  override def (x: T) /%(y: T) = (x / y, x % y)
  override def norm(x: T) = BigInteger(signum(abs(x)))
