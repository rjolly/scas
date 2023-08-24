package scas.structure

import scas.util.{Conversion, unary_~}

trait AbelianGroup[T] extends AbelianGroup.Impl[T] with Structure[T] {
  extension (x: T) {
    inline def + [U: Conversion[T]](y: U) = x.add(~y)
    inline def - [U: Conversion[T]](y: U) = x.subtract(~y)
  }
  extension[U: Conversion[T]] (x: U) {
    inline def + (y: T) = (~x).add(y)
    inline def - (y: T) = (~x).subtract(y)
    inline def unary_- = (~x).negate
  }
  def abs[U: Conversion[T]](x: U) = super.abs(~x)
}

object AbelianGroup {
  trait Impl[T] extends Structure.Impl[T] {
    extension (x: T) {
      def add(y: T): T
      def subtract(y: T): T
      inline def + (y: T) = x.add(y)
      inline def - (y: T) = x.subtract(y)
      inline def unary_- = x.negate
      def negate = zero.subtract(x)
      def isZero = x >< zero
      def signum: Int
    }
    def abs(x: T) = if (x.signum < 0) -x else x
    def zero: T
  }
}
