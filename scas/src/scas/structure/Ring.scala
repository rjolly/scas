package scas.structure

import scas.util.{Conversion, unary_~}
import scas.base.BigInteger
import BigInteger.given

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: BigInteger
  def fromInt(n: BigInteger): T
  def fromInt[U: Conversion[BigInteger]](x: U): T = fromInt(~x)
  def zero = fromInt(0)
  def one = fromInt(1)

  given bigInt2ring: [U: Conversion[BigInteger]] => (U => T) = fromInt[U]
}

object Ring {
  trait Conv[T] extends Ring[T] with AbelianGroup.Conv[T] with Monoid.Conv[T]
}
