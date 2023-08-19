package scas.structure

import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends Structure[T] {
  extension (x: T) {
    def add(y: T): T
    def subtract(y: T): T
    inline def + [U: Conversion[T]](y: U) = x.add(~y)
    inline def - [U: Conversion[T]](y: U) = x.subtract(~y)
    inline def unary_- = x.negate
    def negate = zero.subtract(x)
    def isZero = x >< zero
    def signum: Int
  }
  def abs(x: T) = if (x.signum < 0) -x else x
  def abs[U: Conversion[T]](x: U): T = abs(~x)
  def zero: T
}

object AbelianGroup {
  trait Ops[T] extends Structure.Ops[T] { this: AbelianGroup[T] =>
    extension[U: Conversion[T]] (x: U) {
      inline def + (y: T) = (~x).add(y)
      inline def - (y: T) = (~x).subtract(y)
      inline def unary_- = (~x).negate
    }
  }
}
