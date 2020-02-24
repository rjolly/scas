package scas.structure

trait EuclidianDomain[T] extends UniqueFactorizationDomain[T] with
  def norm(x: T): scas.BigInteger
