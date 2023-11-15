package scas.structure

import scas.base.BigInteger
import BigInteger.given

trait NotQuiteGroup[T] extends Monoid[T] {
  extension (a: T) override def pow(b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else super.pow(a)(b)
  def inverse(x: T): T
}
