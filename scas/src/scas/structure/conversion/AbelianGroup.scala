package scas.structure.conversion

import scala.annotation.targetName
import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends scas.structure.AbelianGroup[T] with Structure[T] {
  extension[U: Conversion[T]] (x: U) {
    def + (y: T) = (~x).add(y)
    def - (y: T) = (~x).subtract(y)
    def unary_- = super.unary_-(~x)
  }
  extension (x: T) {
    @targetName("add") def +[U: Conversion[T]](y: U) = x.add(~y)
    @targetName("subtract") def -[U: Conversion[T]](y: U) = x.subtract(~y)
  }
  def abs[U: Conversion[T]](x: U) = super.abs(~x)
}
