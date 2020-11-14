package scas.structure

import scas.structure.Ring

abstract class Module[T, R: Ring] extends AbelianGroup[T] {
  extension[U] (x: U)(using Conversion[U, R]) def *%(y: T): T = (x: R) *%y
  extension[U] (x: U)(using Conversion[U, T]) def %* (y: R): T = (x: T)%* y
  extension (x: R) def *%(y: T): T
  extension (x: T) def %* (y: R): T
}
