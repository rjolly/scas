package scas.math.conversion

import scala.annotation.targetName
import scas.util.{Conversion, unary_~}

trait Equiv[T] extends scas.math.Equiv[T] {
  extension[U: Conversion[T]] (x: U) {
    def ><(y: T) = equiv(~x, y)
    def <>(y: T) = !equiv(~x, y)
  }
  extension (x: T) {
    @targetName("equiv") def ><[U: Conversion[T]](y: U) = equiv(x, ~y)
    @targetName("nequiv") def <>[U: Conversion[T]](y: U) = !equiv(x, ~y)
  }
}
