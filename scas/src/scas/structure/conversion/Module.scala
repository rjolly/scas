package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait Module[T, R: scas.structure.Ring] extends scas.structure.Module[T, R] with AbelianGroup[T] {
  extension (x: R) abstract override def *%(y: T) = super.*%(x)(y)
  extension[U: Conversion[R]] (x: U) def *%(y: T): T = (~x) *%y
  extension (x: T) {
    abstract override def %* (y: R) = super.%*(x)(y)
    def %* [U: Conversion[R]] (y: U): T = x%* (~y)
  }
}
