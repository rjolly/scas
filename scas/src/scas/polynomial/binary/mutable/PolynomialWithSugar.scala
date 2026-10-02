package scas.polynomial.binary.mutable

class PolynomialWithSugar[T, C](using Polynomial[T, C]) extends scas.polynomial.binary.PolynomialWithSugar.Impl[T, C] with scas.polynomial.mutable.PolynomialWithSugar[T, C, Array[Int]] {
  override given factory: Polynomial[T, C] = summon
}
