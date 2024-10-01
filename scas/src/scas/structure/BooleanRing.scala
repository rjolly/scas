package scas.structure

import scas.util.{Conversion, unary_~}

trait BooleanRing[T] extends Ring[T] {
  extension (x: T) {
    def and(y: T) = x * y
    def or(y: T) = x ^ y ^ (x && y)
    def xor(y: T) = x + y
    def unary_! = x ^ one
    def implies(y: T) = y || !x
    inline def && [U: Conversion[T]](y: U) = x.and(~y)
    inline def || [U: Conversion[T]](y: U) = x.or(~y)
    inline def ^ [U: Conversion[T]](y: U) = x.xor(~y)
    inline def >> [U: Conversion[T]](y: U) = x.implies(~y)
  }
}

object BooleanRing {
  trait Conv[T] extends BooleanRing[T] with Ring.Conv[T] {
    extension[U: Conversion[T]] (x: U) {
      inline def && [V: Conversion[T]](y: V) = (~x).and(~y)
      inline def || [V: Conversion[T]](y: V) = (~x).or(~y)
      inline def ^ [V: Conversion[T]](y: V) = (~x).xor(~y)
      inline def >> [V: Conversion[T]](y: V) = (~x).implies(~y)
    }
  }
}
