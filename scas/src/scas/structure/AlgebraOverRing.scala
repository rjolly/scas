package scas.structure

import scas.structure.Ring

trait AlgebraOverRing[T, R: Ring] extends Module[T, R] with SemiGroup[T]
