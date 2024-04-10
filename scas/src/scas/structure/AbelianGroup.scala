package scas.structure

import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends Structure[T] {
  extension (x: T) {
    def add(y: T): T
    def subtract(y: T): T
    def unary_- = zero - x
    def isZero = x >< zero
    def signum: Int
  }
  extension[U: Conversion[T]] (x: U) {
    inline def + [V: Conversion[T]](y: V) = (~x).add(~y)
    inline def - [V: Conversion[T]](y: V) = (~x).subtract(~y)
  }
  def abs(x: T) = if (x.signum < 0) -x else x
  def abs[U: Conversion[T]](x: U): T = abs(~x)
  def zero: T
}
