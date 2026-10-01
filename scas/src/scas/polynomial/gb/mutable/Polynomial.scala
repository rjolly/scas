package scas.polynomial.gb.mutable

trait Polynomial[T, C, M] extends scas.polynomial.gb.Polynomial[T, C, M] with scas.polynomial.mutable.Polynomial[T, C, M] {
  override def sugar = new PolynomialWithSugar(using this)
}
