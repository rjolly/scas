package scas.structure

import spire.macros.Ops
import scas.Implicits.infixVectorOps

trait VectorSpace[T, R] extends Module[T, R] {
  implicit def self: VectorSpace[T, R]
  implicit def ring: Field[R]
  def rdivide(x: T, y: R) = ltimes(ring.inverse(y), x)
}

object VectorSpace {
  trait ExtraImplicits extends Module.ExtraImplicits {
    implicit def infixVectorOps[T, R](lhs: T)(implicit factory: VectorSpace[T, R]): Ops[T, R] = new OpsImpl[T, R](lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends Module.Element[T, R] { this: T =>
    val factory: VectorSpace[T, R]
    def :/(that: R) = factory.rdivide(this, that)
  }
  trait Ops[T, R] extends Module.Ops[T, R] {
    def :/(rhs: R) = macro Ops.binop[R, T]
  }
  class OpsImpl[T, R](lhs: T)(implicit factory: VectorSpace[T, R]) extends Ops[T, R]
}
