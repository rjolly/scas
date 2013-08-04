package scas.structure

trait Field[@specialized(Int, Long, Double) T] extends EuclidianDomain[T] with NotQuiteGroup[T] {
  override def isUnit(x: T) = !(x.isZero)
  override def divide(x: T, y: T) = x * inverse(y)
  override def remainder(x: T, y: T) = zero
  override def divideAndRemainder(x: T, y: T) = (x / y, x % y)
  override def norm(x: T) = java.lang.Integer.valueOf(signum(abs(x)))
}
