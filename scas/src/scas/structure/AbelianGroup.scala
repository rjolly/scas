package scas.structure

import scas.math.Equiv
import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends Structure[T] {
  extension (x: T) {
    def add(y: T): T
    def subtract(y: T): T
    inline def + [U: Conversion[T]](y: U) = x.add(~y)
    inline def - [U: Conversion[T]](y: U) = x.subtract(~y)
    def unary_- = zero - x
    def isZero = x >< zero
    def signum: Int
  }
  def abs(x: T) = if (x.signum < 0) -x else x
  def abs[U: Conversion[T]](x: U): T = abs(~x)
  def zero: T
}

object AbelianGroup {
  trait Conv[T] extends AbelianGroup[T] with Equiv.Conv[T] {
    extension[U: Conversion[T]] (x: U) {
      inline def + [V: Conversion[T]](y: V) = (~x).add(~y)
      inline def - [V: Conversion[T]](y: V) = (~x).subtract(~y)
    }
  }
}
