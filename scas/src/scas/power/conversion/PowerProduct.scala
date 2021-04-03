package scas.power.conversion

import scala.reflect.ClassTag
import scas.structure.ordered.conversion.Monoid

abstract class PowerProduct[M: ClassTag] extends scas.power.PowerProduct[M] with Monoid[M] {
  given PowerProduct[M] = this
  extension[U] (x: U)(using c: U => M) {
    def / (y: M) = c(x).divide(y)
    def | (y: M) = c(x).factorOf(y)
  }
  extension (x: M) {
    def /[U](y: U)(using c: U => M) = x.divide(c(y))
    def |[U](y: U)(using c: U => M) = x.factorOf(c(y))
  }

  given int2powerProduct: (Int => M) = apply(_)
}
