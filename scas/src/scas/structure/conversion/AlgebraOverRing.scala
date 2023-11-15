package scas.structure.conversion

trait AlgebraOverRing[T, R : scas.structure.Ring] extends scas.structure.AlgebraOverRing[T, R] with Module[T, R] with SemiGroup[T]
