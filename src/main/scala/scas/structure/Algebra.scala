package scas.structure

import scas.Implicits.infixAlgebraOps

trait Algebra[T, R] extends AlgebraOverRing[T, R] with VectorSpace[T, R] {
  implicit def self: Algebra[T, R]
}

object Algebra {
  trait ExtraImplicits extends AlgebraOverRing.ExtraImplicits with VectorSpace.ExtraImplicits {
    implicit def infixAlgebraOps[T, R](lhs: T)(implicit factory: Algebra[T, R]): Ops[T, R] = new OpsImpl[T, R](lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends AlgebraOverRing.Element[T, R] with VectorSpace.Element[T, R] { this: T =>
    val factory: Algebra[T, R]
  }
  trait Ops[T, R] extends AlgebraOverRing.Ops[T, R] with VectorSpace.Ops[T, R]
  class OpsImpl[T, R](lhs: T)(implicit factory: Algebra[T, R]) extends Ops[T, R]
}
