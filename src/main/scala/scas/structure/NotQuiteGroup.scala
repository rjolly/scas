package scas.structure

import scas.BigInteger
import scas.Implicits.{ZZ, infixRingOps}
import BigInteger.signum

trait NotQuiteGroup[@specialized(Int, Long) T] extends Monoid[T] {
  override def pow(x: T, exp: BigInteger) = if (signum(exp) < 0) pow(inverse(x), -exp) else super.pow(x, exp)
  def inverse(x: T): T
}
