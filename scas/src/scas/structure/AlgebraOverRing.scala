package scas.structure

trait AlgebraOverRing[T, R : Ring] extends Module[T, R] with SemiGroup[T]
