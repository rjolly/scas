package scas.structure

import AlgebraOverRing.Element

trait AlgebraOverRing[T <: Element[T, R], R] extends Module[T, R] with SemiGroup[T] {
  override def mkOps(lhs: T): AlgebraOverRing.Ops[T] = throw new UnsupportedOperationException
}

object AlgebraOverRing {
  trait Element[T <: Element[T, R], R] extends Module.Element[T, R] with SemiGroup.Element[T] { this: T =>
    val factory: AlgebraOverRing[T, R]
  }
  trait Ops[T] extends AbelianGroup.Ops[T] with SemiGroup.Ops[T]
}
