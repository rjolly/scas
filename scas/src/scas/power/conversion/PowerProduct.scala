package scas.power.conversion

import scas.structure.ordered.conversion.Monoid
import scas.util.{Conversion, unary_~}

trait PowerProduct[M] extends scas.power.PowerProduct[M] with Monoid[M] {
  extension[U: Conversion[M]] (x: U) {
    inline def / (y: M) = (~x).divide(y)
    inline def | (y: M) = (~x).factorOf(y)
  }
}
