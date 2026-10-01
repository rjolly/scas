package scas.polynomial.binary

import scas.polynomial.BinaryPolynomial

trait Polynomial[T, C] extends scas.polynomial.gb.Polynomial[T, C, Array[Int]] with BinaryPolynomial[T, C] {
  override def sugar = new PolynomialWithSugar(using this)
}
