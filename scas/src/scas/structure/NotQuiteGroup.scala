package scas.structure

import scas.base.conversion.BigInteger
import scas.util.{Conversion, unary_~}
import BigInteger.given

trait NotQuiteGroup[T] extends Monoid[T] {
  extension (a: T) override def pow(b: BigInteger) = if (b.signum < 0) inverse(a) \ -b else super.pow(a)(b)
  def inverse[U: Conversion[T]](x: U): T = inverse(~x)
  def inverse(x: T): T
}
