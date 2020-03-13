package scas.structure.commutative

import scas.BigInteger

trait EuclidianDomain[T] extends UniqueFactorizationDomain[T] with
  def norm(x: T): BigInteger
  def gcd(x: T, y: T) = if (norm(x) < norm(y)) gcd(y, x) else if (y >< zero) x else x % y
