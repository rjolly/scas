package scas.polynomial.gb.mutable

class PolynomialWithSugar[T, C, M](using scas.polynomial.mutable.Polynomial[T, C, M]) extends scas.polynomial.gb.PolynomialWithSugar.Impl[T, C, M] with scas.polynomial.mutable.PolynomialWithSugar[T, C, M]
