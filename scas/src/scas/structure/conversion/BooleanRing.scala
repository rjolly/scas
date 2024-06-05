package scas.structure.conversion

import scas.util.{Conversion, unary_~}

trait BooleanRing[T] extends scas.structure.BooleanRing[T] with Ring[T] {
  extension[U: Conversion[T]] (x: U) {
    inline def && [V: Conversion[T]](y: V) = (~x).and(~y)
    inline def || [V: Conversion[T]](y: V) = (~x).or(~y)
    inline def ^ [V: Conversion[T]](y: V) = (~x).xor(~y)
    inline def >> [V: Conversion[T]](y: V) = (~x).implies(~y)
  }
}
