package scas.structure.impl

import scala.annotation.targetName
import scas.base.BigInteger

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: BigInteger
  @targetName("fromInt") def apply(n: BigInteger): T
}
