package scas.structure

import scas.{BigInteger, int2bigInt}

trait NotQuiteGroup[T] extends Monoid[T] with
  override def (x: T) \ (exp: BigInteger) = if (BigInteger.signum(exp) < 0) inverse(x) \ (-exp) else super.\(x)(exp)
  def inverse(x: T): T
