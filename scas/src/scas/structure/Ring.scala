package scas.structure

import scas.base.BigInteger

trait Ring[T] extends impl.Ring[T] with AbelianGroup[T] with Monoid[T] with AlgebraOverRing[T, BigInteger]
