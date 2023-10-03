package scas.structure.impl

import scas.base.BigInteger

trait Ring[T] extends AbelianGroup[T] with Monoid[T] with AlgebraOverRing[T, BigInteger] {
  def characteristic: BigInteger
  def apply(n: Long): T
}
