package scas.structure.commutative

trait EuclidianDomain[T] extends UniqueFactorizationDomain[T] {
  def gcd(x: T, y: T) = if (y.isZero) x else gcd(y, x % y)
}
