package scas.math.conversion

import scala.annotation.targetName
import scas.util.{Conversion, unary_~}

trait PartialOrdering[T] extends scas.math.PartialOrdering[T] with Equiv[T] {
  extension[U: Conversion[T]] (x: U) {
    def <=(y: T) = lteq(~x, y)
    def >=(y: T) = gteq(~x, y)
    def < (y: T) = lt(~x, y)
    def > (y: T) = gt(~x, y)
  }
  extension (x: T) {
    @targetName("lteq") def <=[U: Conversion[T]](y: U) = lteq(x, ~y)
    @targetName("gteq") def >=[U: Conversion[T]](y: U) = gteq(x, ~y)
    @targetName("lt") def < [U: Conversion[T]](y: U) = lt(x, ~y)
    @targetName("gt") def > [U: Conversion[T]](y: U) = gt(x, ~y)
  }
}
