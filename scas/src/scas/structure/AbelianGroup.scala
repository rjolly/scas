package scas.structure

import scala.language.future
import scas.util.{Conversion, as}

trait AbelianGroup[T] extends Structure[T] {
  extension[U : Conversion[T]] (x: U) {
    def + (y: T) = x.as[T].add(y)
    def - (y: T) = x.as[T].subtract(y)
  }
  extension (x: T) {
    def +[U : Conversion[T]](y: U) = x.add(y.as[T])
    def -[U : Conversion[T]](y: U) = x.subtract(y.as[T])
    def add(y: T): T
    def subtract(y: T): T
    def unary_- = zero - x
  }
  def abs(x: T) = if (x.signum < 0) -x else x
  extension (x: T) def signum: Int
  def zero: T
  extension (x: T) def isZero = x >< zero
}
