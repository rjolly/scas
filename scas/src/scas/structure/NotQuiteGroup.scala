package scas.structure

import scas.{BigInteger, int2bigInt}

trait NotQuiteGroup[T] extends Monoid[T] {
  override def (a: T) \ (b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else super.extension_\(a)(b)
  def inverse(x: T): T
}
