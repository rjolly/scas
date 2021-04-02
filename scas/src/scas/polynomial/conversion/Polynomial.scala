package scas.polynomial.conversion

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.power.PowerProduct

abstract class Polynomial[T : ClassTag, C : Ring, M : PowerProduct] extends scas.polynomial.Polynomial[T, C, M] with scas.structure.conversion.Ring[T] {
  extension (x: T) {
    def %*[U] (m: U)(using c: U => M): T = super.%*(x)(c(m))
  }
}
