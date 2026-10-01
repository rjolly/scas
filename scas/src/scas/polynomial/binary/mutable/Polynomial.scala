package scas.polynomial.binary.mutable

trait Polynomial[T, C] extends scas.polynomial.binary.Polynomial[T, C] with scas.polynomial.gb.mutable.Polynomial[T, C, Array[Int]] {
  override def sugar = new PolynomialWithSugar(using this)
}
