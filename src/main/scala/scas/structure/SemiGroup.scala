package scas.structure

import spire.macros.Ops
import scas.Implicits.infixSemiGroupOps

trait SemiGroup[@specialized(Int, Long, Double) T] extends Structure[T] {
  implicit def self: SemiGroup[T]
  def times(x: T, y: T): T
}

object SemiGroup {
  trait ExtraImplicits extends Structure.ExtraImplicits {
    implicit def infixSemiGroupOps[T: SemiGroup](lhs: T): Ops[T] = new OpsImpl(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Structure.Element[T] { this: T =>
    val factory: SemiGroup[T]
    def *(that: T) = factory.times(this, that)
  }
  trait Ops[T] extends Structure.Ops[T] {
    def *(rhs: T) = macro Ops.binop[T, T]
  }
  class OpsImpl[T: SemiGroup](lhs: T) extends Ops[T]
}
