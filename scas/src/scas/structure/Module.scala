package scas.structure

import scas.structure.Ring

abstract class Module[T, R: Ring] extends AbelianGroup[T] {
  extension[U] (x: U)(using c: U => R) def *%(y: T): T = c(x).multiplyLeft(y)
  extension (x: R) def multiplyLeft(y: T): T
  extension (x: T) {
    def %*[U] (y: U)(using c: U => R): T = x.multiplyRight(c(y))
    def multiplyRight(y: R): T
  }
}
