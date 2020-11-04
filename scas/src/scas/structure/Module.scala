package scas.structure

import scas.structure.Ring

abstract class Module[T, R: Ring] extends AbelianGroup[T] {
  extension (x: R) def *%(y: T): T
  extension (x: T) def %* (y: R): T
}
