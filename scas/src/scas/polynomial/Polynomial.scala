package scas.polynomial

import scala.reflect.ClassTag
import scas.structure.impl.Ring
import scas.power.impl.PowerProduct
import scas.util.{Conversion, unary_~}

trait Polynomial[T : ClassTag, C : Ring, M : PowerProduct] extends impl.Polynomial[T, C, M] with scas.structure.Ring[T] {
  extension (x: T) {
    def %*[U: Conversion[M]] (m: U) = x.ppMultiplyRight(~m)
  }
  given coef2poly[D: Conversion[C]]: (D => T) = x => this(~x)
}
