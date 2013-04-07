package scas.structure

import spire.macros.Ops
import SemiGroup.OpsImpl

trait SemiGroup[@specialized(Int, Long) T] extends Structure[T] {
  def times(x: T, y: T): T
  override implicit def mkOps(lhs: T): SemiGroup.Ops[T] = new OpsImpl(lhs)(this)
}

object SemiGroup {
  trait ExtraImplicits extends Structure.ExtraImplicits {
    implicit def infixSemiGroupOps[T: SemiGroup](lhs: T) = implicitly[SemiGroup[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Structure.Element[T] { this: T =>
    val factory: SemiGroup[T]
    def *(rhs: T) = factory.times(lhs, rhs)
  }
  trait Ops[T] extends Structure.Ops[T] {
    def *(rhs: T) = macro Ops.binop[T, T]
  }
  class OpsImpl[T: SemiGroup](lhs: T) extends Ops[T]
}
