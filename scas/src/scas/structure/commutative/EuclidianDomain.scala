package scas.structure.commutative

trait EuclidianDomain[T] extends EuclidianDomain.Impl[T] with UniqueFactorizationDomain[T]

object EuclidianDomain {
  trait Impl[T] extends UniqueFactorizationDomain.Impl[T] {
    def gcd(x: T, y: T) = if (y.isZero) x else gcd(y, x % y)
  }
}
