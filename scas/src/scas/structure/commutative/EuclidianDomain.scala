package scas.structure.commutative

trait EuclidianDomain[T] extends UniqueFactorizationDomain[T] with
  def norm(x: T): scas.BigInteger
