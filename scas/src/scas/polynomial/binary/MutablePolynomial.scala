package scas.polynomial.binary

trait MutablePolynomial[T, C] extends Polynomial[T, C] with scas.polynomial.gb.MutablePolynomial[T, C, Array[Int]] {
  override def sugar = new MutablePolynomialWithSugar(using this)
}
