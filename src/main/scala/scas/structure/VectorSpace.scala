package scas.structure

import spire.macros.Ops
import VectorSpace.OpsImpl

trait VectorSpace[T, R] extends Module[T, R] {
  implicit val ring: Field[R]
  def rdivide(x: T, y: R) = ring.inverse(y) *: x
  override implicit def mkOps(lhs: T): VectorSpace.Ops[T, R] = new OpsImpl[T, R](lhs)(this)
}

object VectorSpace {
  trait ExtraImplicits extends Module.ExtraImplicits {
    implicit def infixVectorOps[T, R](lhs: T)(implicit factory: VectorSpace[T, R]) = factory.mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends Module.Element[T, R] { this: T =>
    val factory: VectorSpace[T, R]
    def :/(that: R) = factory.rdivide(this, that)
  }
  trait Ops[T, R] extends Module.Ops[T, R] {
    def :/(rhs: R) = macro Ops.binop[R, T]
  }
  class OpsImpl[T, R](lhs: T)(factory: VectorSpace[T, R]) extends Ops[T, R]
}
