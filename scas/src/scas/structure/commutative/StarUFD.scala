package scas.structure.commutative

trait StarUFD[T] extends scas.structure.StarRing[T] with UniqueFactorizationDomain[T] {
  def conjugate(x: T) = magnitude2(x) / x
}
