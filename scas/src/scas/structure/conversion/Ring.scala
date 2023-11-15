package scas.structure.conversion

import scas.base.BigInteger
import scas.util.{Conversion, unary_~}

trait Ring[T] extends scas.structure.Ring[T] with AbelianGroup[T] with Monoid[T] {
  abstract override def fromInt(x: BigInteger) = super.fromInt(x)
  def fromInt[U: Conversion[BigInteger]](x: U): T = fromInt(~x)
}
