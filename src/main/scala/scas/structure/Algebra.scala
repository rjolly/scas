package scas.structure

import Algebra.OpsImpl

trait Algebra[T, R] extends AlgebraOverRing[T, R] with VectorSpace[T, R] {
  override implicit def mkOps(lhs: T): Algebra.Ops[T, R] = new OpsImpl[T, R](lhs)(this)
}

object Algebra {
  trait ExtraImplicits extends AlgebraOverRing.ExtraImplicits with VectorSpace.ExtraImplicits {
    implicit def infixAlgebraOps[T, R](lhs: T)(implicit factory: Algebra[T, R]) = factory.mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends AlgebraOverRing.Element[T, R] with VectorSpace.Element[T, R] { this: T =>
    val factory: Algebra[T, R]
  }
  trait Ops[T, R] extends AlgebraOverRing.Ops[T, R] with VectorSpace.Ops[T, R]
  class OpsImpl[T, R](lhs: T)(factory: Algebra[T, R]) extends Ops[T, R]
}
