package scas.polynomial.gb

import MutablePolynomialWithSugar.Impl

class MutablePolynomialWithSugar[T, C, M](using scas.polynomial.MutablePolynomial[T, C, M]) extends Impl[T, C, M]

object MutablePolynomialWithSugar {
  trait Impl[T, C, M] extends PolynomialWithSugar.Impl[T, C, M] with scas.polynomial.MutablePolynomialWithSugar[T, C, M]
}
