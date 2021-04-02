package scas.polynomial.conversion

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.power.PowerProduct

abstract class Polynomial[T : ClassTag, C : Ring, M : PowerProduct] extends scas.polynomial.Polynomial[T, C, M] with scas.structure.conversion.Ring[T] {
  given Polynomial[T, C, M] = this
  extension (x: T) {
    def %*[U] (m: U)(using c: U => M): T = super.%*(x)(c(m))
  }
  given coef2poly[D](using c: D => C): (D => T) = x => this(c(x))
}
