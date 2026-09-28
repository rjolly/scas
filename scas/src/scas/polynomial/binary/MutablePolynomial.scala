package scas.polynomial.binary

trait MutablePolynomial[T, C] extends Polynomial[T, C] with scas.polynomial.MutablePolynomial[T, C, Array[Int]]
