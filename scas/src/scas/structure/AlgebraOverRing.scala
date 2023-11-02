package scas.structure

import impl.SemiGroup

trait AlgebraOverRing[T, R : Ring] extends Module[T, R] with SemiGroup[T]
