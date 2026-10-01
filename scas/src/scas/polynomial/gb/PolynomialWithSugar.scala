package scas.polynomial.gb

import scas.power.PowerProduct
import PolynomialWithSugar.Impl

class PolynomialWithSugar[T, C, M](using scas.polynomial.Polynomial[T, C, M]) extends Impl[T, C, M]

object PolynomialWithSugar {
  trait Impl[T, C, M] extends scas.polynomial.PolynomialWithSugar[T, C, M] {
    override given pp: PowerProduct[M] = factory.pp
  }
}
