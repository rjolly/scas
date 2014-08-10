package scas.structure

import scas.Implicits.infixAlgebraOverRingOps

trait AlgebraOverRing[T, R] extends Module[T, R] with SemiGroup[T] {
  implicit def self: AlgebraOverRing[T, R]
}

object AlgebraOverRing {
  trait ExtraImplicits extends Module.ExtraImplicits with SemiGroup.ExtraImplicits {
    implicit def infixAlgebraOverRingOps[T, R](lhs: T)(implicit factory: AlgebraOverRing[T, R]): Ops[T, R] = new OpsImpl[T, R](lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends Module.Element[T, R] with SemiGroup.Element[T] { this: T =>
    val factory: AlgebraOverRing[T, R]
  }
  trait Ops[T, R] extends Module.Ops[T, R] with SemiGroup.Ops[T]
  class OpsImpl[T, R](lhs: T)(implicit factory: AlgebraOverRing[T, R]) extends Ops[T, R]
}
