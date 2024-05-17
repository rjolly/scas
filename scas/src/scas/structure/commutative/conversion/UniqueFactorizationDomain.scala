package scas.structure.commutative.conversion

import scas.util.{Conversion, unary_~}

trait UniqueFactorizationDomain[T] extends scas.structure.commutative.UniqueFactorizationDomain[T] with scas.structure.conversion.NotQuiteField[T] {
  extension[U: Conversion[T]] (x: U) {
    inline def % [V: Conversion[T]](y: V) = (~x).remainder(~y)
    inline def /%[V: Conversion[T]](y: V) = (~x).divideAndRemainder(~y)
    inline def | [V: Conversion[T]](y: V) = (~x).factorOf(~y)
  }
}
