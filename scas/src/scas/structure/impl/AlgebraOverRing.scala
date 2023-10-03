package scas.structure.impl

trait AlgebraOverRing[T, R : Ring] extends Module[T, R] with SemiGroup[T]
