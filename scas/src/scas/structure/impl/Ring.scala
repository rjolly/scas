package scas.structure.impl

import scas.base.BigInteger

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: BigInteger
  def fromInt(n: BigInteger): T
}
