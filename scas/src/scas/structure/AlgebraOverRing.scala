package scas.structure

trait AlgebraOverRing[T, R] extends Module[T, R] with SemiGroup[T]

object AlgebraOverRing {
  trait Conv[T, R : scas.structure.Ring] extends AlgebraOverRing[T, R] with AbelianGroup.Conv[T] with SemiGroup.Conv[T]
}
