package scas.polynomial.gb.mutable

import PolynomialWithSugar.Impl

class PolynomialWithSugar[T, C, M](using scas.polynomial.mutable.Polynomial[T, C, M]) extends Impl[T, C, M]

object PolynomialWithSugar {
  trait Impl[T, C, M] extends scas.polynomial.gb.PolynomialWithSugar.Impl[T, C, M] with scas.polynomial.mutable.PolynomialWithSugar[T, C, M]
}
