package scas.structure.conversion

trait AlgebraOverRing[T, R : scas.structure.Ring] extends scas.structure.AlgebraOverRing[T, R] with AbelianGroup[T] with SemiGroup[T]
