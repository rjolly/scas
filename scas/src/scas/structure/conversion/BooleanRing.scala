package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait BooleanRing[T] extends scas.structure.BooleanRing[T] with Ring[T] {
  extension (x: T) {
    inline def && [U: Conversion[T]](y: U) = x.and(~y)
    inline def || [U: Conversion[T]](y: U) = x.or(~y)
    inline def ^ [U: Conversion[T]](y: U) = x.xor(~y)
    inline def >> [U: Conversion[T]](y: U) = x.implies(~y)
  }
  extension[U: Conversion[T]] (x: U) {
    inline def && [V: Conversion[T]](y: V) = (~x).and(~y)
    inline def || [V: Conversion[T]](y: V) = (~x).or(~y)
    inline def ^ [V: Conversion[T]](y: V) = (~x).xor(~y)
    inline def >> [V: Conversion[T]](y: V) = (~x).implies(~y)
  }
}
