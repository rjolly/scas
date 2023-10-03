package scas.structure.impl

import scas.base.BigInteger

trait Ring[T] extends AbelianGroup[T] with Monoid[T] with AlgebraOverRing[T, BigInteger] {
  def characteristic: BigInteger
  extension (x: BigInteger) def multiplyLeft(y: T) = y.multiplyRight(x)
  extension (ring: Ring[BigInteger]) def pow(n: Int) = {
    assert (false)
    this
  }
  def apply(n: Long): T
}
