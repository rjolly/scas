package scas.polynomial.gb

trait MutablePolynomial[T, C, M] extends Polynomial[T, C, M] with scas.polynomial.MutablePolynomial[T, C, M] {
  override def sugar = new MutablePolynomialWithSugar(using this)
}
