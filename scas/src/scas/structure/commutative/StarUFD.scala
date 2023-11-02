package scas.structure.commutative

trait StarUFD[T] extends scas.structure.impl.StarRing[T] with UniqueFactorizationDomain[T] {
  def conjugate(x: T) = magnitude2(x) / x
}
