package scas.structure

import AlgebraOverRing.OpsImpl

trait AlgebraOverRing[T, R] extends Module[T, R] with SemiGroup[T] {
  override implicit def mkOps(lhs: T): AlgebraOverRing.Ops[T, R] = new OpsImpl[T, R](lhs)(this)
}

object AlgebraOverRing {
  trait ExtraImplicits extends Module.ExtraImplicits with SemiGroup.ExtraImplicits {
    implicit def infixAlgebraOverRingOps[T, R](lhs: T)(implicit factory: AlgebraOverRing[T, R]) = factory.mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T, R], R] extends Module.Element[T, R] with SemiGroup.Element[T] { this: T =>
    val factory: AlgebraOverRing[T, R]
  }
  trait Ops[T, R] extends Module.Ops[T, R] with SemiGroup.Ops[T]
  class OpsImpl[T, R](lhs: T)(factory: AlgebraOverRing[T, R]) extends Ops[T, R]
}
