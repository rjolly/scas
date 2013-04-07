package scas.structure

import scas.BigInteger
import Ring.OpsImpl

trait Ring[@specialized(Int, Long) T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: BigInteger
  override implicit def mkOps(lhs: T): Ring.Ops[T] = new OpsImpl(lhs)(this)
}

object Ring {
  trait ExtraImplicits extends AbelianGroup.ExtraImplicits with Monoid.ExtraImplicits {
    implicit def infixRingOps[T: Ring](lhs: T) = implicitly[Ring[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends AbelianGroup.Element[T] with Monoid.Element[T] { this: T =>
    val factory: Ring[T]
  }
  trait Ops[T] extends AbelianGroup.Ops[T] with Monoid.Ops[T]
  class OpsImpl[T: Ring](lhs: T) extends Ops[T]
}
