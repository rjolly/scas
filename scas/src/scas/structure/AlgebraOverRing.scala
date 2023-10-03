package scas.structure

trait AlgebraOverRing[T, R : impl.Ring] extends impl.AlgebraOverRing[T, R] with Module[T, R] with SemiGroup[T]
