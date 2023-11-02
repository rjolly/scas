package scas.structure.conversion

import scas.structure.SemiGroup

trait AlgebraOverRing[T, R : scas.structure.impl.Ring] extends scas.structure.AlgebraOverRing[T, R] with Module[T, R] with SemiGroup[T]
