package scas.polynomial.conversion

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.power.PowerProduct
import scas.util.{Conversion, unary_~}

trait Polynomial[T : ClassTag, C : Ring, M : PowerProduct] extends scas.polynomial.Polynomial[T, C, M] with scas.structure.conversion.Ring[T] {
  given Polynomial[T, C, M] = this
  extension (x: T) {
    def %*[U: Conversion[M]] (m: U): T = super.%*(x)(~m)
  }
  given coef2poly[D: Conversion[C]]: (D => T) = x => this(~x)
}
