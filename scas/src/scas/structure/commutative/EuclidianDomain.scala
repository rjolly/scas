package scas.structure.commutative

trait EuclidianDomain[T] extends UniqueFactorizationDomain[T] {
  def gcd(x: T, y: T) = if (y >< zero) x else gcd(y, x % y)
}
