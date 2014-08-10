package scas.structure

import scas.BigInteger
import scas.Implicits.infixRingOps

trait Ring[@specialized(Int, Long, Double) T] extends AbelianGroup[T] with Monoid[T] {
  implicit def self: Ring[T]
  def characteristic: BigInteger
}

object Ring {
  trait ExtraImplicits extends AbelianGroup.ExtraImplicits with Monoid.ExtraImplicits {
    implicit def infixRingOps[T: Ring](lhs: T): Ops[T] = new OpsImpl(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends AbelianGroup.Element[T] with Monoid.Element[T] { this: T =>
    val factory: Ring[T]
  }
  trait Ops[T] extends AbelianGroup.Ops[T] with Monoid.Ops[T]
  class OpsImpl[T: Ring](lhs: T) extends Ops[T]
}
