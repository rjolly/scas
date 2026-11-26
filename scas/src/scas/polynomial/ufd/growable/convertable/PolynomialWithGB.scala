package scas.polynomial.ufd.growable.convertable

import scala.compiletime.deferred
import scas.polynomial.ConvertablePolynomial
import scas.structure.commutative.UniqueFactorizationDomain
import scas.power.growable.ArrayPowerProduct

trait PolynomialWithGB[T[N], C, N] extends scas.polynomial.ufd.growable.PolynomialWithGB[T, C, Array[N]] with scas.polynomial.ufd.convertable.PolynomialWithGB[T, C, N] {
  given pp: ArrayPowerProduct[N] = deferred

  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T[Array[N]]*): PolynomialWithGB[T, C, N] = {
    same(s*)
    this
  }
}
