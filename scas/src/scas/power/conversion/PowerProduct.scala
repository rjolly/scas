package scas.power.conversion

import scala.reflect.ClassTag
import scala.annotation.targetName
import scas.structure.ordered.conversion.Monoid
import scas.util.{Conversion, unary_~}

trait PowerProduct[M: ClassTag] extends scas.power.PowerProduct[M] with Monoid[M] {
  given PowerProduct[M] = this
  extension[U: Conversion[M]] (x: U) {
    def / [V: Conversion[M]](y: V) = (~x).divide(~y)
    def | [V: Conversion[M]](y: V) = (~x).factorOf(~y)
  }
  extension (x: M) {
    @targetName("divide") def / [U: Conversion[M]](y: U) = x.divide(~y)
    @targetName("factorOf") def | [U: Conversion[M]](y: U) = x.factorOf(~y)
  }

  given int2powerProduct: (Int => M) = apply(_)
}
