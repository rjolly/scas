package scas.structure

import scas.BigInteger

trait NotQuiteGroup[T] extends Monoid[T] {
  extension (a: T) override def \ (b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else super.\(a)(b)
  def inverse(x: T): T
}
