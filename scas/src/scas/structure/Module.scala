package scas.structure

import impl.{Ring, AbelianGroup}

trait Module[T, R : Ring] extends AbelianGroup[T] {
  extension (x: R) def multiplyLeft(y: T): T
  extension (x: T) def multiplyRight(y: R): T
  extension (x: R) def *%(y: T) = x.multiplyLeft(y)
  extension (x: T) def %* (y: R) = x.multiplyRight(y)

  extension (ring: Ring[R]) def pow(n: Int): Module[T, R]
}
