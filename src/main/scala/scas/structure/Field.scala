package scas.structure

import Ordering.Implicits.infixOrderingOps

trait Field[T] extends EuclidianDomain[T] with NotQuiteGroup[T] {
  override def gcd(x: T, y: T) = if (norm(x) < norm(y)) y else x
  override def lcm(x: T, y: T) = if (norm(x) > norm(y)) y else x
  override def isUnit(x: T) = !(x.isZero)
  override def remainder(x: T, y: T) = zero
  override def divideAndRemainder(x: T, y: T) = (x / y, x % y)
  def norm(x: T) = java.math.BigInteger.valueOf(signum(abs(x)))
  def inverse(x: T) = one / x
}
