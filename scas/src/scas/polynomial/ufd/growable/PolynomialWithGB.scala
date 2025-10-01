package scas.polynomial.ufd.growable

import scala.compiletime.deferred
import scas.power.growable.ArrayPowerProduct
import scas.module.Array

trait PolynomialWithGB[T, C, N] extends PolynomialOverUFD[T, C, Array[N]] with scas.polynomial.ufd.PolynomialWithGB[T, C, N] {
  given pp: ArrayPowerProduct[N] = deferred
}
