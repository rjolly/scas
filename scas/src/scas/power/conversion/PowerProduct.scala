package scas.power.conversion

import scala.reflect.ClassTag
import scas.structure.ordered.conversion.Monoid
import scas.util.{Conversion, unary_~}

trait PowerProduct[M: ClassTag] extends scas.power.PowerProduct[M] with Monoid[M] {
  given PowerProduct[M] = this
  extension[U: Conversion[M]] (x: U) {
    inline def / (y: M) = (~x).divide(y)
    inline def | (y: M) = (~x).factorOf(y)
  }
  extension (x: M) {
    inline def / [U: Conversion[M]](y: U) = x.divide(~y)
    inline def | [U: Conversion[M]](y: U) = x.factorOf(~y)
  }

  given int2powerProduct: (Int => M) = apply(_)
}
