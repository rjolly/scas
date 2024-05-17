package scas.structure

import scas.base.BigInteger
import scas.util.{Conversion, unary_~}

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: BigInteger
  def fromInt(n: BigInteger): T
  def fromInt[U: Conversion[BigInteger]](x: U): T = fromInt(~x)
}
