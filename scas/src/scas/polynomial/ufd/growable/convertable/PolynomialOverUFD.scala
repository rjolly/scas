package scas.polynomial.ufd.growable.convertable

import scala.compiletime.deferred
import scas.polynomial.ConvertablePolynomial
import scas.structure.commutative.UniqueFactorizationDomain
import scas.power.growable.splitable.ArrayPowerProduct

trait PolynomialOverUFD[T, C, N] extends scas.polynomial.ufd.growable.PolynomialOverUFD[T, C, Array[N]] with ConvertablePolynomial[T, C, N] {
  given pp: ArrayPowerProduct[N] = deferred

  extension (ring: UniqueFactorizationDomain[C]) override def apply(s: T*): PolynomialOverUFD[T, C, N] = {
    same(s*)
    this
  }
}
