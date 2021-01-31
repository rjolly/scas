package scas.structure

import scas.structure.Ring

abstract class Module[T, R: Ring] extends AbelianGroup[T] {
  extension[U] (x: U)(using c: U => R) def *%(y: T): T = c(x) *%y
  extension (x: R) def *%(y: T): T
  extension (x: T) {
    def %*[U] (y: U)(using c: U => R): T = x%* c(y)
    def %* (y: R): T
  }
}
